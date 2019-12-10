pub fn run() -> String {
  let comp = icc::Computer::load(0, &loader::load_integer_row_list("./day/02/input.csv")[0]);
  let result = icc::input::run(&comp, Some(icc::input::Input::new(12, 2)));

  format!("{}", result)
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn truth() {
    let comp = Computer::load(&loader::load_integer_row_list("./day/02/input.csv")[0]);
    let actual = comp.run(Some(Input::new(12, 2)));

    assert_eq!(actual, 6_627_023);
  }
}
