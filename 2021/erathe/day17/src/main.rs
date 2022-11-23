use std::ops::RangeInclusive;

#[macro_use]
extern crate lazy_static;

lazy_static! {
    static ref TARGET: (RangeInclusive<i32>, RangeInclusive<i32>) =
        ((185i32..=221i32), (-122i32..=-74i32));
    static ref MIN_Y: i32 = -122;
    static ref MAX_X: i32 = 221;
}

fn main() {
    let mut hits = Vec::new();
    for i in 0..=*MAX_X {
        for j in *MIN_Y..1000 {
            let mut b = Bullet::new(i, j);
            while b.get_x() < *MAX_X && b.get_y() > *MIN_Y {
                b.update_pos();
                b.decrease_speed();

                if b.in_target() {
                    hits.push(b.max_y);
                    break;
                }
            }
        }
    }
    let res = hits.iter().max().unwrap();
    println!("{:?}", res);
    println!("{:?}", hits.len());
}

#[derive(Debug)]
struct Bullet {
    position: (i32, i32),
    velocity: (i32, i32),
    max_y: i32,
}

impl Bullet {
    fn new(x: i32, y: i32) -> Self {
        Bullet {
            position: (0, 0),
            velocity: (x, y),
            max_y: 0,
        }
    }
    fn in_target(&self) -> bool {
        TARGET.0.contains(&self.position.0) && TARGET.1.contains(&self.position.1)
    }
    fn update_pos(&mut self) {
        self.position.0 += self.velocity.0;
        self.position.1 += self.velocity.1;
        self.max_y = self.max_y.max(self.position.1);
    }
    fn decrease_speed(&mut self) {
        self.velocity.0 = (self.velocity.0 - 1).max(0);
        self.velocity.1 -= 1;
    }
    fn get_x(&self) -> i32 {
        self.position.0
    }
    fn get_y(&self) -> i32 {
        self.position.1
    }
}
