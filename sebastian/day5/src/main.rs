use anyhow;
use std::collections::HashMap;

type Crate = char;

#[derive(Default, Debug)]
struct Move {
    from: usize,
    to: usize,
    count: usize,
}

#[derive(Default, Debug)]
struct Pile {
    crates: Vec<Crate>,
}

impl Pile {
    fn add_to_bottom(&mut self, c: Crate) {
        self.crates.insert(0, c);
    }
    
    fn take_top(&mut self) -> Option<Crate> {
        self.crates.pop()
    }
    
    fn add_top(&mut self, c: Crate) {
        self.crates.push(c);
    }
    
    fn add_pile(&mut self, cs: &mut Vec<Crate>) {
        self.crates.append(cs);
    }
    
    fn peek_top(&self) -> Option<&Crate> {
        self.crates.last()
    }
}

#[derive(Default, Debug)]
struct CargoPiles {
    piles: HashMap<usize, Pile>,
    nums: Vec<usize>,
}

impl CargoPiles {
    fn add_on_bottom(&mut self, pile_idx: usize, c: Crate) {
        match self.piles.get_mut(&pile_idx) {
            Some(pile) => pile.add_to_bottom(c),
            None => {
                let pile = Pile { crates: vec![c] };
                self.piles.insert(pile_idx, pile);
            }
        }
    }

    fn execute_moves(&mut self, moves: Vec<Move>) -> anyhow::Result<()> {
        for m in moves {
            for _ in 1..=m.count {
                let c =
                    self.piles.get_mut(&m.from)
                        .and_then(Pile::take_top)
                        .map(Result::Ok)
                        .unwrap_or_else(|| anyhow::bail!("Expected being able to take from pile"))?;

                self.piles
                    .get_mut(&m.to)
                    .map(|p| Ok(p.add_top(c)))
                    .unwrap_or_else(|| anyhow::bail!("Expected to find pile to add to"))?;
            }
        }
        Ok(())
    }
    
    fn execute_moves_9001(&mut self, moves: Vec<Move>) -> anyhow::Result<()> {
        for m in moves {
            let mut crate_buffer = vec![];
            for _ in 1..=m.count {
                let c =
                    self.piles.get_mut(&m.from)
                        .and_then(Pile::take_top)
                        .map(Result::Ok)
                        .unwrap_or_else(|| anyhow::bail!("Expected being able to take from pile"))?;
                crate_buffer.insert(0, c);
            }
            self.piles
                .get_mut(&m.to)
                .map(|p| Ok(p.add_pile(&mut crate_buffer)))
                .unwrap_or_else(|| anyhow::bail!("Expected to find pile to add to"))?;
        }
        Ok(())
    }
    
    fn get_tops(&self) -> Vec<Crate> {
        let mut crates = vec![];
        for key in self.nums.iter() {
            if let Some(c) = self.piles.get(key).and_then(Pile::peek_top) {
                crates.push(c.clone());
            } else {
                panic!("Missing crate on top of pile!");
            }
        }
        crates
    }
}

mod parser {
    use crate::{CargoPiles, Crate, Move};
    use nom::branch::alt;
    use nom::bytes::complete::tag;
    use nom::character::complete::digit1;
    use nom::multi::separated_list1;
    use nom::sequence::delimited;
    use nom::{
        character::complete::{anychar, char},
        IResult,
    };
    use nom::bytes::complete::take_until;
    use nom::sequence::terminated;

    fn filled_crate(i: &str) -> IResult<&str, Option<Crate>> {
        let (i, c) = delimited(char('['), anychar, char(']'))(i)?;
        Ok((i, Some(c)))
    }

    fn crate_void(i: &str) -> IResult<&str, Option<Crate>> {
        let (i, _) = tag("   ")(i)?;
        Ok((i, None))
    }

    fn crate_line(i: &str) -> IResult<&str, Vec<Option<Crate>>> {
        separated_list1(char(' '), alt((filled_crate, crate_void)))(i)
    }

    fn crate_num(i: &str) -> IResult<&str, usize> {
        let (i, _) = char(' ')(i)?;
        let (i, num) = digit1(i)?;
        let (i, _) = char(' ')(i)?;
        Ok((i, num.parse::<usize>().expect("Parsed integer to be integer")))
    }

    fn crate_rows(i: &str) -> IResult<&str, Vec<Vec<Option<Crate>>>> {
        separated_list1(char('\n'), crate_line)(i)
    }

    fn crate_nums(i: &str) -> IResult<&str, Vec<usize>> {
        separated_list1(char(' '), crate_num)(i)
    }

    fn eol(i: &str) -> IResult<&str, ()> {
        let (i, _) = terminated(take_until("\n"), tag("\n"))(i)?;
        Ok((i, ()))
    }
        
    pub(crate) fn initial_piles(i: &str) -> IResult<&str, CargoPiles> {
        let mut config = CargoPiles::default();
        
        let (i, rows) = crate_rows(i)?;
        let (i, _) = eol(i)?;
        let (i, nums) = crate_nums(i)?;
        let (i, _) = eol(i)?;
        let (i, _) = eol(i)?;
        
        for row in rows {
            for (c, idx) in row.into_iter().zip(&nums) {
                match c {
                    Some(c) => config.add_on_bottom(*idx, c),
                    None => (), // No crate to add to this pile...
                }
            }
        }
        
        config.nums = nums;
        Ok((i, config))
    }

    fn parse_move(i: &str) -> IResult<&str, Move> {
        let (i, _) = tag("move ")(i)?;
        let (i, count) = digit1(i)?;
        let (i, _) = tag(" from ")(i)?;
        let (i, from) = digit1(i)?;
        let (i, _) = tag(" to ")(i)?;
        let (i, to) = digit1(i)?;
        
        Ok((i, Move {
            count: count.parse::<usize>().expect("Successful parsed integer"),
            from: from.parse::<usize>().expect("Successful parsed integer"),
            to: to.parse::<usize>().expect("Successful parsed integer"),
        }))
    }
    
    pub(crate) fn moves(i: &str) -> IResult<&str, Vec<Move>> {
        separated_list1(char('\n'), parse_move)(i)
    }

    #[cfg(test)]
    mod test {
        use crate::parser::{crate_line, crate_nums, crate_void, filled_crate, initial_piles, moves, parse_move};
        use crate::{Move};

        #[test]
        fn parse_single_crate() {
            let (_, p) = filled_crate("[A]").unwrap();
            assert_eq!(Some('A'), p);

            let (_, p) = crate_void("   ").unwrap();
            assert_eq!(None, p);
        }

        #[test]
        fn multiple_crates() {
            let (_, res) = crate_line("[A]     [B]").unwrap();
            assert_eq!(vec![Some('A'), None, Some('B')], res);
        }

        #[test]
        fn crate_ids() {
            let (_, res) = crate_nums(" 1   2   3  ").unwrap();
            assert_eq!(vec![1, 2, 3], res);
        }
        
        #[test]
        fn parse_start_config() {
            let input = include_str!("../sample.txt");
            let (_, res) = initial_piles(input).unwrap();
            assert_eq!(res.piles.len(), 3);
            assert_eq!(res.piles.get(&1).unwrap().crates, vec!['Z', 'N']);
            assert_eq!(res.piles.get(&2).unwrap().crates, vec!['M', 'C', 'D']);
            assert_eq!(res.piles.get(&3).unwrap().crates, vec!['P']);
        }
        
        #[test]
        fn parse_move_test() {
            let (_, Move {from, to, count}) = parse_move("move 1 from 2 to 3").unwrap();
            assert_eq!(count, 1);
            assert_eq!(from, 2);
            assert_eq!(to, 3);
        }
        
        #[test]
        fn parse_moves() {
            let input = include_str!("../sample.txt");
            let (i, _) = initial_piles(input).unwrap();
            let (_, moves) = moves(i).unwrap();
            assert_eq!(moves.len(), 4);
        }
    }
}

fn main() -> anyhow::Result<()> {
    let i = include_str!("../input.txt");
    let (i, mut game) = parser::initial_piles(i)?;
    let (_, moves) = parser::moves(i)?;
    game.execute_moves(moves)?;
    println!("Top crates: {:?}", game.get_tops().iter().collect::<String>());

    let i = include_str!("../input.txt");
    let (i, mut game) = parser::initial_piles(i)?;
    let (_, moves) = parser::moves(i)?;
    game.execute_moves_9001(moves)?;
    println!("Top crates (9001): {:?}", game.get_tops().iter().collect::<String>());
    
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{parser};

    #[test]
    fn create_starting_state() {
        let i = include_str!("../sample.txt");
        let (i, mut game) = parser::initial_piles(i).unwrap();
        let (_, moves) = parser::moves(i).unwrap();
        game.execute_moves(moves).unwrap();
        assert_eq!(game.get_tops(), vec!['C', 'M', 'Z']);
    }
    
    #[test]
    fn create_starting_state_9001() {
        let i = include_str!("../sample.txt");
        let (i, mut game) = parser::initial_piles(i).unwrap();
        let (_, moves) = parser::moves(i).unwrap();
        game.execute_moves_9001(moves).unwrap();
        assert_eq!(game.get_tops(), vec!['M', 'C', 'D']);
    }
}
