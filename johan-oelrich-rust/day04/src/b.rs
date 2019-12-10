pub fn run() -> String {
  format!("{}", naive_count())
}

const PASSWORD_LENGTH: usize = 6;
const PASSWORD_MIN: u32 = 125_730;
const PASSWORD_MAX: u32 = 579_381;
#[derive(PartialEq, Eq, Debug)]
enum Result {
  Low,
  High,
  Good,
  Bad,
}

fn naive_count() -> u32 {
  let mut password = [1, 2, 5, 7, 3, 0];
  let mut possible = 0;
  loop {
    match is_valid(password) {
      Result::Good => possible += 1,
      Result::High => break,
      _ => (),
    }
    increase(&mut password);
  }
  possible
}

fn increase(password: &mut [u8; PASSWORD_LENGTH]) {
  password[5] += 1;
  password[4] += password[5] / 10;
  password[5] %= 10;
  password[3] += password[4] / 10;
  password[4] %= 10;

  password[2] += password[3] / 10;
  password[3] %= 10;

  password[1] += password[2] / 10;
  password[2] %= 10;

  password[0] += password[1] / 10;
  password[1] %= 10;
}

fn is_valid(password: [u8; PASSWORD_LENGTH]) -> Result {
  let mut has_double = false;
  let mut streak = 1;
  for idx in 1..PASSWORD_LENGTH {
    if password[idx - 1] > password[idx] {
      return Result::Bad;
    } else if password[idx - 1] == password[idx] {
      streak += 1;
    } else if streak > 1 {
      if !has_double {
        has_double = streak == 2;
      }
      streak = 1;
    }
  }
  let value = password_value(password);
  if value <= PASSWORD_MIN {
    return Result::Low;
  }
  if value >= PASSWORD_MAX {
    return Result::High;
  }
  if streak == 2 {
    has_double = true;
  }
  if !has_double {
    return Result::Bad;
  }
  Result::Good
}

fn password_value(given: [u8; PASSWORD_LENGTH]) -> u32 {
  given
    .iter()
    .rev()
    .enumerate()
    .map(|(idx, v)| *v as u32 * 10u32.pow(idx as u32))
    .sum()
}

#[cfg(test)]
mod tests {
  use super::*;
  #[ignore]
  #[test]
  fn truth() {
    assert_eq!(naive_count(), 2081);
  }
  #[test]
  fn increase_1() {
    let mut number = [1, 2, 3, 4, 5, 9];
    increase(&mut number);
    assert_eq!(number, [1, 2, 3, 4, 6, 0]);
  }
  #[test]
  fn increase_2() {
    let mut number = [8, 9, 9, 9, 9, 9];
    increase(&mut number);
    assert_eq!(number, [9, 0, 0, 0, 0, 0]);
  }
  #[test]
  fn good_value() {
    assert_eq!(password_value([1, 2, 3, 4, 5, 6]), 123_456);
  }
  #[test]
  fn count_1() {
    // assert_eq!(count_passwords(), 5);
  }
  #[test]
  fn valid_1() {
    assert_eq!(is_valid([2, 2, 3, 3, 4, 4]), Result::Good);
  }
  #[test]
  fn valid_2() {
    assert_eq!(is_valid([2, 3, 3, 4, 6, 7]), Result::Good);
  }
  #[test]
  fn valid_3() {
    assert_eq!(is_valid([2, 2, 2, 2, 3, 3]), Result::Good);
  }
  #[test]
  fn valid_4() {
    assert_eq!(is_valid([2, 2, 3, 4, 4, 4]), Result::Good);
  }
  #[test]
  fn valid_5() {
    assert_eq!(is_valid([2, 2, 2, 3, 3, 4]), Result::Good);
  }
  #[test]
  fn invalid_1() {
    assert_eq!(is_valid([2, 2, 3, 4, 5, 0]), Result::Bad);
  }
  #[test]
  fn invalid_2() {
    assert_eq!(is_valid([2, 3, 4, 6, 7, 9]), Result::Bad);
  }
  #[test]
  fn invalid_3() {
    assert_eq!(is_valid([1, 1, 1, 1, 1, 1]), Result::Low);
  }
  #[test]
  fn invalid_4() {
    assert_eq!(is_valid([6, 6, 6, 6, 6, 6]), Result::High);
  }
  #[test]
  fn invalid_5() {
    assert_eq!(is_valid([2, 2, 2, 3, 3, 3]), Result::Bad);
  }
  #[test]
  fn invalid_7() {
    assert_eq!(is_valid([3, 3, 3, 3, 3, 3]), Result::Bad);
  }
}
