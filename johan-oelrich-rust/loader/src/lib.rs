use std::fs::File;
use std::io::{prelude::*, BufReader};

fn read_as_iter(path: &str) -> impl Iterator<Item = String> {
  let file = File::open(path).expect("should have found the file");
  let reader = BufReader::new(file);
  reader.lines().map(Result::unwrap)
}

pub fn load_as<T>(path: &str, converter: fn(String) -> T) -> impl Iterator<Item = T> {
  read_as_iter(path).map(converter)
}

pub fn load_integer_list(path: &str) -> Vec<i64> {
  let targets: Vec<i64> = load_as(path, |s| {
    s.trim().parse().expect("should have been a number ...")
  })
  .collect();
  targets
}
pub fn load_integer_row_list(path: &str) -> Vec<Vec<i64>> {
  let targets: Vec<Vec<i64>> = read_as_iter(path)
    .map(|s| {
      s.trim()
        .split(',')
        .map(|e| e.parse().expect("should have been a number ..."))
        .collect()
    })
    .collect();
  targets
}
