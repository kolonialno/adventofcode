use std::str::Lines;

pub fn solve_a(boarding_passes: &Vec<BoardingPass>) -> u32 {
    boarding_passes.iter().map(|bp| bp.seat_id()).max().unwrap()
}

pub fn solve_b(boarding_passes: &Vec<BoardingPass>) -> u32 {
    let seat_ids = boarding_passes
        .iter()
        .map(|bp| bp.seat_id().to_owned())
        .collect::<Vec<u32>>();
    let min = seat_ids.iter().min().unwrap().to_owned();
    let max = seat_ids.iter().max().unwrap().to_owned();
    for seat_id in min..=max {
        if seat_ids.iter().any(|&s| s == seat_id - 1)
            && !seat_ids.iter().any(|&s| s == seat_id)
            && seat_ids.iter().any(|&s| s == seat_id + 1)
        {
            return seat_id;
        }
    }
    0
}

#[derive(Debug)]
pub struct BoardingPass(String);

impl BoardingPass {
    pub fn from_lines(lines: Lines) -> Vec<BoardingPass> {
        lines.map(|line| BoardingPass(line.to_owned())).collect()
    }

    pub fn row(&self) -> u8 {
        let mut row = 0;
        for (i, c) in (&self.0[..7]).chars().enumerate() {
            match c {
                'F' => {}
                'B' => row += 2u8.pow((6 - i) as u32),
                _ => panic!("Unexpected char"),
            }
        }
        row
    }

    pub fn column(&self) -> u8 {
        let mut column = 0;
        for (i, c) in (&self.0[7..]).chars().enumerate() {
            match c {
                'L' => {}
                'R' => column += 2u8.pow((2 - i) as u32),
                _ => panic!("Unexpected char"),
            }
        }
        column
    }

    pub fn seat_id(&self) -> u32 {
        self.row() as u32 * 8 + self.column() as u32
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_a() {
        let input = String::from(
            r"BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL",
        );
        let boarding_passes = BoardingPass::from_lines(input.lines());
        assert_eq!(solve_a(&boarding_passes), 820);
    }

    #[test]
    fn test_row() {
        let boarding_pass = BoardingPass("BFFFBBFRRR".to_owned());
        assert_eq!(boarding_pass.row(), 70);
    }

    #[test]
    fn test_column() {
        let boarding_pass = BoardingPass("BFFFBBFRRR".to_owned());
        assert_eq!(boarding_pass.column(), 7);
    }

    #[test]
    fn test_seat_id() {
        let boarding_pass = BoardingPass("BFFFBBFRRR".to_owned());
        assert_eq!(boarding_pass.seat_id(), 567);
    }
}
