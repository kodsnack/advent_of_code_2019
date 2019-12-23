use std::path::PathBuf;

mod lib;

fn main() {
    let result: u32 = lib::spif::parse_pixels(vector(8));
    println!("{}", result);
}

fn vector<'a>(day: u8) -> Vec<u32> {
    let file_data: String = load_file(day);
    file_data.chars().filter_map(|c| c.to_digit(10)).collect()
}

fn load_file(day: u8) -> String {
    let file_name: String = format!("inputs/{}", day);
    let path = PathBuf::from(file_name);
    std::fs::read_to_string(path).expect("Could not read file")
}
