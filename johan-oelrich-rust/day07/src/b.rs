pub fn run() -> String {
  let program = loader::load_integer_row_list("./day/07/input.csv")[0].clone();
  let settings = vec![5, 6, 7, 8, 9];
  let result = mathy::permutations_in_memory(settings)
    .iter()
    .map(|setting| {
      let mut amps = Amplifiers::new(&program, &setting);
      amps.run_to_end(0)
    })
    .max()
    .unwrap();
  format!("best: {}", result)
}

const AMPLIFIER_COUNT: usize = 5;

use icc::helpers::State;
pub struct Amplifiers {
  amplifiers: Vec<icc::Computer>,
  buffer: Vec<(usize, i64)>,
  send_to: Vec<usize>,
}
impl Amplifiers {
  pub fn new(program: &[i64], pss: &[i64]) -> Amplifiers {
    Amplifiers {
      amplifiers: (0..AMPLIFIER_COUNT)
        .map(|id| icc::Computer::load(id, program))
        .collect(),
      buffer: pss.iter().cloned().enumerate().collect(),
      send_to: (0..AMPLIFIER_COUNT)
        .map(|dest| (dest + 1) % AMPLIFIER_COUNT)
        .collect(),
    }
  }
  fn post_messsage(&mut self, from: usize, message: i64) {
    assert!(self.send_to.len() > from);
    self.buffer.push((self.send_to[from], message))
  }

  fn print_buffer(&self) {
    println!("buffer depth: {}", self.buffer.len());
    self
      .buffer
      .iter()
      .cloned()
      .for_each(|(id, msg)| println!("({}, {})", id, msg));
  }

  fn run(&mut self) -> Option<i64> {
    let mut halted: std::collections::HashSet<usize> = (0..AMPLIFIER_COUNT).collect();
    let mut idx = 0;
    loop {
      match self.amplifiers[idx].run() {
        State::Crashed => panic!("amplifier {} crashed", idx),
        State::Halted => {
          halted.remove(&idx);
        }
        State::Ready => (),
        State::Running => (),
        State::Output(id, msg) => {
          println!("{} sending {}", id, msg);
          self.post_messsage(id, msg)
        }
        State::Input => {
          if let Some(pos) = self.buffer.iter().position(|(addr, _value)| *addr == idx) {
            let (_addr, value) = self.buffer.remove(pos);
            if !self.amplifiers[idx].input_value(value) {
              panic!("{} asked for input and then refused it", idx)
            }
          }
        }
      }
      idx += 1;
      idx %= AMPLIFIER_COUNT;
      if halted.is_empty() {
        return self.buffer.pop().map(|(_id, val)| val);
      }
    }
  }
  pub fn run_to_end(&mut self, init: i64) -> i64 {
    self.buffer.push((0, init));
    self.print_buffer();
    self.run().unwrap()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn test_1() {
    let program = vec![
      3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28,
      1005, 28, 6, 99, 0, 0, 5,
    ];
    let mut amps = Amplifiers::new(&program, &[9, 8, 7, 6, 5]);
    let actual = amps.run_to_end(0);
    assert_eq!(actual, 139_629_729);
  }
  #[test]
  fn test_2() {
    let program = vec![
      3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54, -5,
      54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4, 53, 1001,
      56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10,
    ];
    let mut amps = Amplifiers::new(&program, &[9, 7, 8, 5, 6]);
    let actual = amps.run_to_end(0);
    assert_eq!(actual, 18_216);
  }
}
