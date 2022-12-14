use std::{
    env,
    ops::{Add, Sub},
    str::FromStr,
};

pub fn get_input_file() -> String {
    let dir = env::current_exe().unwrap();
    let exe = dir
        .file_name()
        .unwrap()
        .to_ascii_lowercase()
        .into_string()
        .unwrap();

    format!(
        "{}/input.txt",
        exe.chars().skip(exe.len() - 2).collect::<String>()
    )
}
