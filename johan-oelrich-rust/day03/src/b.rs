pub fn run() -> String {
  format!("{}", wire::solve_day03b())
}
#[cfg(test)]
mod tests {
  #[test]
  fn truth() {
    let actual = wire::solve_day03b();

    assert_eq!(actual, 7534);
  }
}
