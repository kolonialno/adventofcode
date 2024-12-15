use crate::utils::read_to_lines;

fn solve() {
    let equations = read_to_lines("data/7.txt");
    let mut sum_1 = 0;
    let mut sum_2 = 0;
    for equation in equations {
        let result = equation.split(":").collect::<Vec<&str>>()[0]
            .parse::<i128>()
            .unwrap();
        let numbers = equation.split(":").collect::<Vec<&str>>()[1]
            .split(" ")
            .map(|x| x.parse::<i128>().unwrap_or(0))
            .collect::<Vec<i128>>()[1..]
            .to_vec();

        if match_1(numbers[0], &numbers, 1, result) {
            sum_1 += result
        }
        if match_2(numbers[0], &numbers, 1, result) {
            sum_2 += result
        }
    }
    println!("Sum task 1: {}", sum_1);
    println!("Sum task 2: {}", sum_2);
}

fn match_1(a: i128, equation: &Vec<i128>, index: usize, target: i128) -> bool {
    // Since we only have addition/multiplication, we can return if a>res
    if a > target {
        return false;
    }

    // If we are at last index we check for a match
    if index == equation.len() - 1 {
        if (a + equation[index] == target) | (a * equation[index] == target) {
            return true;
        } else {
            return false;
        }
    }

    // If not at end, call function recursively for both operators
    match_1(a + equation[index], equation, index + 1, target)
        | match_1(a * equation[index], equation, index + 1, target)
}

fn match_2(a: i128, equation: &Vec<i128>, index: usize, target: i128) -> bool {
    // Since we only have addition/multiplication, we can return if a>res
    if a > target {
        return false;
    }

    // If we are at last index we check for a match
    if index == equation.len() - 1 {
        if (a + equation[index] == target)
            | (a * equation[index] == target)
            | (concatenate(&a, &equation[index]) == target)
        {
            return true;
        } else {
            return false;
        }
    }

    // If not at end, call function recursively for both operators
    match_2(a + equation[index], equation, index + 1, target)
        | match_2(a * equation[index], equation, index + 1, target)
        | match_2(
            concatenate(&a, &equation[index]),
            equation,
            index + 1,
            target,
        )
}

fn concatenate(a: &i128, b: &i128) -> i128 {
    [a.to_string(), b.to_string()]
        .join("")
        .parse::<i128>()
        .unwrap()
}

pub fn __main__() {
    solve();
}
