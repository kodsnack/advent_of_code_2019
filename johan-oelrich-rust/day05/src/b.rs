use loader::load_integer_row_list;

pub fn run() -> String {
  let mut comp = icc::Computer::load(0, &load_integer_row_list("./day/05/input.csv")[0]);
  let result = icc::run_to_end(&mut comp, &[5]);
  format!("{:?}", result)
}

#[cfg(test)]
mod tests {
  use loader::load_integer_row_list;

  #[test]
  fn truth() {
    let mut comp = icc::Computer::load(0, &load_integer_row_list("../day/05/input.csv")[0]);
    let result = icc::run_to_end(&mut comp, &[5]);
    assert_eq!(result, vec![7873292]);
  }
}
