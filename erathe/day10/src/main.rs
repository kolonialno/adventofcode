const SCREEN_SIZE: usize = 40;

fn main() {
    let input = include_str!("../input.txt");
    let mut screen = Screen::default();
    let mut commands = input
        .lines()
        .map(|l| match l.as_bytes()[0] {
            b'n' => Command::new_noop(),
            b'a' => Command::new_addx(l.split_once(" ").unwrap().1.parse::<i32>().unwrap()),
            _ => panic!("malformed input"),
        })
        .peekable();

    let mut res = 0;
    loop {
        // Enter cycle, check if we need to process a new command.
        if screen.current_command.is_none() {
            screen.current_command = commands.next();
        }

        // During cycle
        // part 1 lol
        match screen.current_cycle {
            19 => res += 20 * screen.register_x,
            59 => res += 60 * screen.register_x,
            99 => res += 100 * screen.register_x,
            139 => res += 140 * screen.register_x,
            179 => res += 180 * screen.register_x,
            219 => res += 220 * screen.register_x,
            _ => (),
        }

        //part 2
        screen.draw();

        // "After" cycle
        screen.process_command();

        // check if we have any more commands to process
        if commands.peek().is_none() {
            break;
        }
        screen.current_cycle += 1;
    }
    // part 1
    dbg!(res);

    // part 2
    for row in screen.display {
        println!("{:?}", row.iter().collect::<String>());
    }
}

struct Screen {
    register_x: i32,
    current_command: Option<Command>,
    current_cycle: i32,
    cycles_processed: i32,
    display: [[char; 40]; 6],
}

impl Screen {
    fn process_command(&mut self) {
        self.cycles_processed += 1;
        if let Some(command) = &self.current_command {
            if command.cycles == self.cycles_processed {
                match command.c_type {
                    CommandType::Addx(v) => self.register_x += v,
                    CommandType::Noop => (),
                }
                self.current_command = None;
                self.cycles_processed = 0;
            }
        };
    }

    fn draw(&mut self) {
        if self
            .register_x
            .abs_diff(self.current_cycle % SCREEN_SIZE as i32)
            <= 1
        {
            self.display[self.current_cycle as usize / SCREEN_SIZE]
                [self.current_cycle as usize % SCREEN_SIZE] = '#';
        }
    }
}

impl Default for Screen {
    fn default() -> Self {
        Self {
            register_x: 1,
            current_command: None,
            cycles_processed: 0,
            current_cycle: 0,
            display: [[' '; 40]; 6],
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
