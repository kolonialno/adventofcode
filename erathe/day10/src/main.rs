const SCREEN_SIZE: usize = 40;

fn main() {
    let input = include_str!("../input.txt");
    let mut screen = Screen::default();
    let mut commands = input.lines().map(|l| match l.as_bytes()[0] {
        b'n' => Command::new_noop(),
        b'a' => Command::new_addx(l.strip_prefix("addx ").unwrap().parse::<i32>().unwrap()),
        _ => panic!("malformed input"),
    });

    while let Some(command) = commands.next() {
        screen.process_command(command);
        screen.execute_command();
    }
    // part 1
    println!("part 1: {:?}", screen.temp_buffer);

    // part 2
    println!("part 2:");
    for row in screen.display {
        println!("{:?}", row.iter().collect::<String>());
    }
}

struct Screen {
    register_x: i32,
    temp_buffer: i32,
    current_command: Option<CommandType>,
    current_cycle: i32,
    rem_inst_cycles: i32,
    display: [[char; 40]; 6],
}

impl Screen {
    fn process_command(&mut self, command: Command) {
        self.rem_inst_cycles = command.cycles;
        self.current_command = Some(command.c_type);
    }

    fn execute_command(&mut self) {
        for _ in 0..self.rem_inst_cycles {
            self.tick();
        }
        if let Some(command) = &self.current_command {
            match command {
                CommandType::Addx(v) => self.register_x += v,
                CommandType::Noop => (),
            }
            self.current_command = None;
        }
    }

    fn tick(&mut self) {
        if (self.current_cycle + 21) % SCREEN_SIZE as i32 == 0 {
            self.temp_buffer += (self.current_cycle + 1) * self.register_x;
        }
        self.draw();
        self.current_cycle += 1;
    }

    fn draw(&mut self) {
        if self
            .register_x
            .abs_diff(self.current_cycle % SCREEN_SIZE as i32)
            <= 1
        {
            self.display[(self.current_cycle as usize / SCREEN_SIZE)]
                [(self.current_cycle as usize % SCREEN_SIZE)] = '█';
        }
    }
}

impl Default for Screen {
    fn default() -> Self {
        Self {
            register_x: 1,
            display: [[' '; 40]; 6],
            current_command: None,
            current_cycle: 0,
            rem_inst_cycles: 0,
            temp_buffer: 0,
        }
    }
}

struct Command {
    c_type: CommandType,
    cycles: i32,
}

impl Command {
    fn new_addx(val: i32) -> Self {
        Self {
            c_type: CommandType::Addx(val),
            cycles: 2,
        }
    }

    fn new_noop() -> Self {
        Self {
            c_type: CommandType::Noop,
            cycles: 1,
        }
    }
}

enum CommandType {
    Noop,
    Addx(i32),
}
