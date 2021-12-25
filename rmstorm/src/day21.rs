use std::collections::HashMap;

fn reg_play(player: &mut [u32; 2], die: &mut dyn Iterator<Item = u32>) -> bool {
    player[0] += die.take(3).sum::<u32>();
    player[0] = ((player[0] - 1) % 10) + 1;
    player[1] += player[0];
    if player[1] >= 1000 {
        println!("p1 {:?}", player);
        return true;
    }
    false
}

fn branch_play(
    mut players: [[u64; 2]; 2],
    turn: usize,
    wins: &mut [u64; 2],
    die_value: u64,
    num_branches: u64,
    branches: &HashMap<u64, u64>,
) {
    players[turn][0] += die_value;
    players[turn][0] = ((players[turn][0] - 1) % 10) + 1;
    players[turn][1] += players[turn][0];
    if players[turn][1] >= 21 {
        wins[turn] += num_branches;
        // wins[turn] += 1;
    } else {
        for (next_die_value, extra_num_branches) in branches.iter() {
            branch_play(
                players,
                (turn + 1) % 2,
                wins,
                *next_die_value,
                (*extra_num_branches) * num_branches,
                branches,
            )
        }
    }
}

pub fn day21() {
    let mut die = (1..101).cycle();
    let mut player1: [u32; 2] = [3, 0];
    let mut player2: [u32; 2] = [10, 0];

    let mut playing = true;
    let mut turn = 0;
    while playing {
        turn += 1;
        if turn % 2 == 1 {
            if reg_play(&mut player1, &mut die) {
                println!("answer1 {:?}", player2[1] * turn * 3);
                playing = false;
            }
        } else {
            if reg_play(&mut player2, &mut die) {
                println!("answer1 {:?}", player1[1] * turn * 3);
                playing = false;
            }
        };
    }

    let players: [[u64; 2]; 2] = [[3, 0], [10, 0]];
    let mut branches: HashMap<u64, u64> = HashMap::new();
    for i in 1..4 {
        for ii in 1..4 {
            for iii in 1..4 {
                *branches.entry(i + ii + iii).or_insert(0) += 1;
            }
        }
    }
    let mut wins = [0, 0];
    for (die_value, num_branches) in branches.iter() {
        branch_play(players, 0, &mut wins, *die_value, *num_branches, &branches)
        // branch_play(players, 0, &mut wins, *die_value, &branches)
    }
    println!("answer2 {:?}", wins);
}
