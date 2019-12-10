pub fn run() -> String {
  let program = loader::load_integer_row_list("input.csv")[0].clone();
  let mut computer = icc::Computer::load(0, &program);
  let result = icc::run_to_end(&mut computer, &[1]);
  match result {
    Ok(data) => format! {"{:?}", data},
    Err(error) => error,
  }
}
#[cfg(test)]
mod test {
  #[test]
  fn name() {
    // 3638931938
  }
}
