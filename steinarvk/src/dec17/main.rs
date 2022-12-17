type Result<T> = std::result::Result<T, anyhow::Error>;

use std::collections::HashMap;

use std::io::Read;

struct Map<T> {
    number_of_rows: i32,
    number_of_columns: i32,
    row_data: Vec<Vec<T>>,
}

impl Map<char> {
    fn from_string(s: &str) -> Result<Map<char>> {
        let rows: Vec<&str> = s.trim().split('\n').collect();
        let number_of_rows = rows.len();
        let number_of_cols = rows[0].len();

        if !(rows.iter().all(|s| s.trim().len() == number_of_cols)) {
            anyhow::bail!("rows are not all the same length");
        }

        let row_data = rows
            .iter()
            .map(|row| row.trim().chars().collect())
            .collect();

        Ok(Map {
            number_of_rows: number_of_rows.try_into().unwrap(),
            number_of_columns: number_of_cols.try_into().unwrap(),
            row_data,
        })
    }
}

impl<T> Map<T>
where
    T: Clone,
{
    fn new(number_of_columns: i32, number_of_rows: i32, starting_value: &T) -> Map<T> {
        let row_data = (0..number_of_rows)
            .map(|_| {
                (0..number_of_columns)
                    .map(|_| starting_value.clone())
                    .collect()
            })
            .collect();
        Map {
            number_of_rows,
            number_of_columns,
            row_data,
        }
    }

    fn at(&self, (col, row): (i32, i32)) -> Option<&T> {
        if row < 0 || row >= self.number_of_rows || col < 0 || col >= self.number_of_columns {
            return None;
        }
        Some(&self.row_data[row as usize][col as usize])
    }

    fn at_mut(&mut self, (col, row): (i32, i32)) -> Option<&mut T> {
        if row < 0 || row >= self.number_of_rows || col < 0 || col >= self.number_of_columns {
            return None;
        }
        Some(&mut self.row_data[row as usize][col as usize])
    }

    fn indexed_for_each<F>(&self, mut f: F)
    where
        F: FnMut((i32, i32), &T),
    {
        for (y, row) in self.row_data.iter().enumerate() {
            for (x, value) in row.iter().enumerate() {
                f((x.try_into().unwrap(), y.try_into().unwrap()), value);
            }
        }
    }
}

fn drop_block(
    map: &mut Map<char>,
    block: &Map<char>,
    jets: &[char],
    jet_index: &mut usize,
    tower_height: i32,
) -> Result<(i32, Option<i32>)> {
    let from_bottom = tower_height + 3 + block.number_of_rows;

    let mut x = 2;
    let mut y = map.number_of_rows - from_bottom;

    let mut new_max_y = map.number_of_rows;

    loop {
        // Move by jet if possible
        let (ddx, ddy) = match jets[*jet_index] {
            '>' => (1, 0),
            '<' => (-1, 0),
            _ => panic!("oops"),
        };
        *jet_index = (*jet_index + 1) % jets.len();

        let mut is_blocked = false;
        block.indexed_for_each(|(dx, dy), v| {
            if *v != '.' {
                if match map.at((x + dx + ddx, y + dy + ddy)) {
                    Some('.') => false,
                    _ => true,
                } {
                    is_blocked = true;
                }
            }
        });
        if !is_blocked {
            x += ddx;
            y += ddy;
        }

        // Move down if possible
        let (ddx, ddy) = (0, 1);
        let mut is_blocked = false;
        block.indexed_for_each(|(dx, dy), v| {
            if *v != '.' {
                if match map.at((x + dx + ddx, y + dy + ddy)) {
                    Some('.') => false,
                    _ => true,
                } {
                    is_blocked = true;
                }
            }
        });
        if !is_blocked {
            x += ddx;
            y += ddy;
        } else {
            break;
        }
    }

    // freeze
    block.indexed_for_each(|(dx, dy), v| {
        if *v != '.' {
            new_max_y = new_max_y.min(y + dy);
            *map.at_mut((x + dx, y + dy)).unwrap() = *v;
        }
    });

    let mut full_row: Option<i32> = None;

    for dy in 0..block.number_of_rows {
        let y = y + (block.number_of_rows - 1 - dy);
        let row_is_full = (0..map.number_of_columns).all(|x| *map.at((x, y)).unwrap() != '.');
        if row_is_full {
            let at_tower_height = map.number_of_rows - y;
            full_row = Some(at_tower_height);
        }
    }

    let new_tower_height = map.number_of_rows - new_max_y;
    let new_tower_height = new_tower_height.max(tower_height);

    return Ok((new_tower_height, full_row));
}

fn scan_top(map: &Map<char>, tower_height: i32) -> Option<(u128, bool)> {
    // I haven't found a proof that the state of the tower more than 18 rows
    // below the top can't affect the rest of the sequeence (which would
    // make it not a true repeat).
    // Note that if we require a full row, then it's easy to prove.
    // But not all inputs produce full rows -- notably the test input.. diabolical!
    // (In contrast, my real input DOES produce full rows, so it seems
    // provable that this method -- when restricting to full rows -- gets
    // the correct answer.);
    let rows = 18;
    if tower_height < rows {
        return None;
    }

    let y0 = map.number_of_rows - tower_height;
    let y1 = map.number_of_rows - (tower_height - rows);

    assert!((y1 - y0) == rows);

    let mut rv: u128 = 0;
    let mut totalcount = 0;
    let mut has_full_row = false;

    for y in y0..y1 {
        let mut count = 0;
        for x in 0..7 {
            rv <<= 1;
            totalcount += 1;
            if *map.at((x, y)).unwrap() != '.' {
                rv += 1;
                count += 1;
            }
        }
        assert!(count > 0);
        if count == 7 {
            has_full_row = true;
        }
    }
    assert!(totalcount == 126);

    Some((rv, has_full_row))
}

fn main() -> Result<()> {
    let blocks = vec![
        Map::from_string("0000")?,
        Map::from_string(".1.\n111\n.1.")?,
        Map::from_string("..2\n..2\n222")?,
        Map::from_string("3\n3\n3\n3")?,
        Map::from_string("44\n44")?,
    ];
    let mut buffer = String::new();
    std::io::stdin().read_to_string(&mut buffer)?;
    let jets: Vec<char> = buffer.trim().chars().collect();

    for require_full_row in vec![true, false] {
        let mut map: Map<char> = Map::new(7, 1_000_000, &'.');
        let mut tower_height = 0;
        let mut jet_index = 0;
        let mut blocks_dropped: i64 = 0;

        let mut repeats: HashMap<(u8, usize, u128), (i64, i64)> = HashMap::new();

        let mut calculated_tower_heights: Vec<i32> = Vec::new();

        let mut found_answer_one = false;
        let mut found_answer_two = false;

        while !found_answer_one || !found_answer_two {
            calculated_tower_heights.push(tower_height);

            let block = &blocks[(blocks_dropped as usize) % blocks.len()];
            let (new_tower_height, _) =
                drop_block(&mut map, &block, &jets, &mut jet_index, tower_height)?;
            tower_height = new_tower_height;

            if tower_height > (map.number_of_rows - 5) {
                break;
            }

            blocks_dropped += 1;
            if blocks_dropped == 2022 {
                found_answer_one = true;
                println!(
                    "Tower height after {} blocks dropped: {}",
                    blocks_dropped, tower_height
                );
            }

            if !found_answer_two {
                if let Some((shape, has_full_row)) = scan_top(&map, tower_height) {
                    if require_full_row && !has_full_row {
                        continue;
                    }

                    let block_index: u8 =
                        (blocks_dropped % (blocks.len() as i64)).try_into().unwrap();
                    let key = (block_index, jet_index, shape);

                    if let Some((last_tower_height, last_blocks_dropped)) = repeats.get(&key) {
                        let delta_blocks: i64 = blocks_dropped - last_blocks_dropped;
                        let delta_height: i64 = (tower_height as i64) - last_tower_height;
                        let target: i64 = 1000000000000;

                        let mut accumulated_height: i64 = tower_height.into();
                        let mut left_to_target = target - blocks_dropped;

                        while left_to_target > delta_blocks {
                            let mut shift = 0;
                            while left_to_target > (delta_blocks << (shift + 1)) {
                                shift += 1;
                            }
                            left_to_target -= delta_blocks << shift;
                            accumulated_height += delta_height << shift;
                        }

                        let j = *last_blocks_dropped;
                        let k = last_blocks_dropped + left_to_target;
                        let final_delta = calculated_tower_heights[k as usize]
                            - calculated_tower_heights[j as usize];
                        println!(
                            "Answer at {}: {}",
                            target,
                            accumulated_height + (final_delta as i64)
                        );

                        found_answer_two = true;
                    }

                    repeats.insert(key, (tower_height.into(), blocks_dropped));
                }
            }
        }

        if found_answer_one && found_answer_two {
            break;
        }

        eprintln!("Cannot find answer with full rows. Degrading to non-proven solution (still extremely likely to be correct.");
    }

    Ok(())
}
