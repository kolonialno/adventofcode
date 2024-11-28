use anyhow::bail;
use std::collections::HashMap;

type Result<T> = std::result::Result<T, anyhow::Error>;

fn main() -> Result<()> {
    let disk_size: i64 = 70000000;
    let required_free_space: i64 = 30000000;

    let mut path_stack: Vec<String> = vec!["/".to_string()];
    let mut dir_size: HashMap<String, i64> = HashMap::new();

    for line in std::io::stdin().lines() {
        let line = line?;
        let tokens: Vec<&str> = line.trim().split(' ').collect();

        match &tokens[..] {
            ["$", "cd", "/"] => {
                path_stack = vec!["/".to_string()];
            }
            ["$", "cd", ".."] => {
                assert!(path_stack.len() > 1);
                path_stack.pop();
            }
            ["$", "cd", subdirectory] => {
                let current_path: &str = path_stack.last().unwrap();
                path_stack.push(format!(
                    "{}{}{}",
                    current_path,
                    if current_path != "/" { "/" } else { "" },
                    subdirectory
                ));
            }
            ["$", "ls"] => (),
            ["dir", _directory_name] => (),
            [] => (),
            [file_size, _file_name] if file_size.chars().all(char::is_numeric) => {
                let file_size = file_size.parse::<i64>()?;
                for ancestor_dir in &path_stack {
                    dir_size
                        .entry(ancestor_dir.to_string())
                        .and_modify(|x| *x += file_size)
                        .or_insert(file_size);
                }
            }
            _ => {
                bail!("Unexpected line: {} (parsed as tokens: {:?})", line, tokens);
            }
        }
    }

    for (k, v) in dir_size.iter() {
        eprintln!("[info] {}\t{}", v, k);
    }

    let total_used_space = dir_size.get("/").unwrap_or(&0);
    eprintln!("[info] total used disk space: {}", total_used_space);

    let current_free_space = disk_size - total_used_space;
    let required_extra_space = required_free_space - current_free_space;

    eprintln!("[info] required extra space: {}", required_extra_space);

    println!(
        "Sum of <100k dir sizes: {}",
        dir_size.values().filter(|sz| **sz <= 100_000).sum::<i64>()
    );

    println!(
        "Size of smallest directory with size >={}: {}",
        required_extra_space,
        dir_size
            .values()
            .filter(|sz| **sz >= required_extra_space)
            .min()
            .expect("expected a big-enough directory to exist; no solution to puzzle"),
    );

    Ok(())
}
