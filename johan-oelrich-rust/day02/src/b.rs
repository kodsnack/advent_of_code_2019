pub fn run() -> String {
  let comp = icc::Computer::load(0, &loader::load_integer_row_list("./day/02/input.csv")[0]);
  let result = icc::input::find_cent(&comp, 19_690_720).expect("should give some value");
  format!("{}", result)
}
#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn truth() {
    let comp = Computer::load(&loader::load_integer_row_list("./day/02/input.csv")[0]);
    let actual = comp.find_cent(19_690_720).expect("should give some value");

    assert_eq!(actual, 4019);
  }
}
