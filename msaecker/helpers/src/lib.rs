use std::{env, io};

pub fn get_input_file() -> io::Result<String> {
    let dir = env::current_exe()?;
    let exe = dir
        .file_name()
        .unwrap()
        .to_ascii_lowercase()
        .into_string()
        .unwrap();

    Ok(format!(
        "{}/input.txt",
        exe.chars().skip(exe.len() - 2).collect::<String>()
    ))
}
