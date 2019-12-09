use std::collections::HashSet;

pub fn find_cent(computer: &super::Computer, target: i64) -> Option<i64> {
  let mut tested = HashSet::new();
  while let Some(input) = Input::get_next(&mut tested) {
    if run(computer, Some(input.clone())) == target {
      return Some(input.cent());
    }
  }
  None
}
pub fn run(computer: &super::Computer, input: Option<Input>) -> i64 {
  let mut computer = computer.clone();
  if let Some(input) = input {
    computer.set(1, input.noun);
    computer.set(2, input.verb);
  }
  assert!(super::run_to_end(&mut computer, &[]).is_ok());
  computer.read_memory(0).unwrap()
}
trait InputAvailable {
  fn has_input(&self) -> Option<Input>;
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub struct Input {
  noun: i64,
  verb: i64,
}
impl Input {
  fn next(&mut self) {
    self.verb += 1;
    if self.verb == 100 {
      self.verb = 0;
      self.noun += 1;
    }
    if self.noun == 100 {
      self.noun = 0;
    }
  }
  pub fn cent(&self) -> i64 {
    self.noun * 100 + self.verb
  }
  fn in_range(val: i64) {
    assert!(val >= 0 && val <= 99);
  }
  pub fn new(noun: i64, verb: i64) -> Input {
    Input::in_range(noun);
    Input::in_range(verb);
    Input { noun, verb }
  }
  fn get_next(taken: &mut HashSet<Input>) -> Option<Input> {
    if taken.len() == 100 * 100 {
      return None;
    }
    let mut input = Input::new(0, 0);
    while !taken.insert(input.clone()) {
      input.next();
    }
    Some(input)
  }
}
