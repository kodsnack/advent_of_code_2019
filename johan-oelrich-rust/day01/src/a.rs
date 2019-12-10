pub fn run() -> String {
  format!(
    "{}",
    loader::load_integer_list("./day/01/input.csv").fuel_need()
  )
}

trait FuelNeed {
  fn fuel_need(&self) -> i64;
}

impl FuelNeed for Vec<i64> {
  fn fuel_need(&self) -> i64 {
    self.iter().fold(0, |s, v| s + fuel_requirement(*v))
  }
}

fn fuel_requirement(module_weight: i64) -> i64 {
  (module_weight / 3) - 2
}

#[cfg(test)]
mod tests {
  use super::{fuel_requirement, FuelNeed};
  #[test]
  fn truth() {
    assert_eq!(
      loader::load_integer_list("./day/01/input.csv").fuel_need(),
      3_457_681
    );
  }
  #[test]
  fn values() {
    let values = vec![12, 14, 1969, 100_756];
    let expected = vec![2, 2, 654, 33_583];
    let actual = values
      .iter()
      .map(|v| fuel_requirement(*v))
      .collect::<Vec<i64>>();
    assert_eq!(actual, expected);
  }
  #[test]
  fn total() {
    let values = vec![12, 14, 1969, 100_756];
    let expected = 2 + 2 + 654 + 33_583;
    assert_eq!(values.fuel_need(), expected)
  }
}
