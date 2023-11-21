use std::collections::HashSet;
type Result<T> = std::result::Result<T, anyhow::Error>;

#[derive(Debug, Clone, Copy)]
struct Vector2D {
    x: i32,
    y: i32,
}

impl Vector2D {
    fn new(x: i32, y: i32) -> Vector2D {
        Vector2D { x, y }
    }

    fn add(&mut self, other: &Vector2D) {
        self.x += other.x;
        self.y += other.y;
    }

    fn sub(&mut self, other: &Vector2D) {
        self.x -= other.x;
        self.y -= other.y;
    }

    fn chebyshev_length(&self) -> i32 {
        i32::max(self.x.abs(), self.y.abs())
    }

    fn as_tuple(&self) -> (i32, i32) {
        (self.x, self.y)
    }
}

#[derive(Debug)]
struct Rope {
    head_offset: Vector2D,
    tail_segment: Vector2D,
    child: Option<Box<Rope>>,
}

#[derive(Debug, Copy, Clone)]
enum Direction {
    Up,
    Right,
    Left,
    Down,
}

impl Direction {
    fn as_vector(&self) -> Vector2D {
        match self {
            Direction::Up => Vector2D { x: 0, y: 1 },
            Direction::Right => Vector2D { x: 1, y: 0 },
            Direction::Down => Vector2D { x: 0, y: -1 },
            Direction::Left => Vector2D { x: -1, y: 0 },
        }
    }
}

fn clip(x: i32, lower: i32, upper: i32) -> i32 {
    i32::max(i32::min(x, upper), lower)
}

impl Rope {
    fn of_length(length: i32) -> Rope {
        assert!(length >= 2); // Every rope has at least a head and tail
        let mut segment: Rope = Rope::new(None);

        for _ in 2..length {
            segment = Rope::new(Some(Box::new(segment)));
        }

        segment
    }

    fn new(child: Option<Box<Rope>>) -> Rope {
        Rope {
            head_offset: Vector2D::new(0, 0),
            tail_segment: Vector2D::new(0, 0),
            child,
        }
    }

    fn move_head(&mut self, delta: &Vector2D) {
        if delta.chebyshev_length() == 0 {
            return;
        }

        assert!(delta.chebyshev_length() == 1);

        self.head_offset.add(delta);

        if self.head_offset.chebyshev_length() > 1 {
            let tail_step = Vector2D::new(
                clip(self.head_offset.x, -1, 1),
                clip(self.head_offset.y, -1, 1),
            );

            self.tail_segment.add(&tail_step);
            self.head_offset.sub(&tail_step);

            if let Some(ch) = &mut self.child {
                (*ch).move_head(&tail_step);
            }
        }
    }

    fn tail_position(&self) -> Vector2D {
        match &self.child {
            None => self.tail_segment,
            Some(ch) => (*ch).tail_position(),
        }
    }
}

fn main() -> Result<()> {
    let mut short_rope = Rope::of_length(2);
    let mut long_rope = Rope::of_length(10);

    let mut short_tailcount: HashSet<(i32, i32)> = HashSet::new();
    let mut long_tailcount: HashSet<(i32, i32)> = HashSet::new();

    short_tailcount.insert((0, 0));
    long_tailcount.insert((0, 0));

    for line in std::io::stdin().lines() {
        let line = line?;
        let tokens: Vec<&str> = line.trim().split(' ').collect();

        if let [direction, steps] = &tokens[..] {
            let steps = steps.parse::<i64>()?;
            let direction = match *direction {
                "U" => Direction::Up,
                "D" => Direction::Down,
                "R" => Direction::Right,
                "L" => Direction::Left,
                _ => panic!("bad direction {}", direction),
            };

            for _ in 0..steps {
                short_rope.move_head(&direction.as_vector());
                short_tailcount.insert(short_rope.tail_position().as_tuple());

                long_rope.move_head(&direction.as_vector());
                long_tailcount.insert(long_rope.tail_position().as_tuple());
            }
        } else {
            panic!("unexpected input: {}", line);
        }
    }

    println!("Tail count of short rope: {}", short_tailcount.len());
    println!("Tail count of long rope: {}", long_tailcount.len());

    Ok(())
}
