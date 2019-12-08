pub mod b;
// pub mod a {
//   use super::*;
//   pub fn run() -> String {
//     let program = loader::load_integer_row_list("./day/07/input.csv")[0].clone();
//     let result = permutations_in_memory()
//       .iter()
//       .map(|setting| {
//         let mut amps = Amplifiers::new(&program);
//         amps.test_setting(*setting)
//       })
//       .max()
//       .unwrap();
//     format!("best: {}", result)
//   }
// }

// use std::collections::HashSet;
// fn permutations_from_set(
//   item: [i64; AMPLIFIER_COUNT],
//   set: HashSet<i64>,
// ) -> Vec<[i64; AMPLIFIER_COUNT]> {
//   if set.is_empty() {
//     return vec![item];
//   }
//   set
//     .iter()
//     .cloned()
//     .flat_map(|d| {
//       let mut next_item = item;
//       next_item[AMPLIFIER_COUNT - set.len()] = d;
//       let mut next_set = set.clone();
//       next_set.remove(&d);
//       permutations_from_set(next_item, next_set)
//     })
//     .collect()
// }
// fn permutations_in_memory() -> Vec<[i64; AMPLIFIER_COUNT]> {
//   permutations_from_set(
//     [0; AMPLIFIER_COUNT],
//     (0..AMPLIFIER_COUNT).map(|d| d as i64).collect(),
//   )
// }

// const AMPLIFIER_COUNT: usize = 5;

// pub struct Amplifiers {
//   amplifiers: Vec<icc::Computer>,
// }
// impl Amplifiers {
//   pub fn new(program: &[i64]) -> Amplifiers {
//     Amplifiers {
//       amplifiers: vec![icc::Computer::load(program); AMPLIFIER_COUNT],
//     }
//   }
//   pub fn test_setting(&mut self, pss: [i64; AMPLIFIER_COUNT]) -> i64 {
//     self
//       .amplifiers
//       .iter_mut()
//       .enumerate()
//       .for_each(|(idx, amp)| amp.set_input(&[pss[idx]]));
//     self.amplifiers.iter_mut().fold(0, |input, amp| {
//       amp.add_input(input);
//       let result = amp.run_to_end();
//       assert_eq!(result.len(), 1);
//       result[0]
//     })
//   }
// }

// #[cfg(test)]
// mod tests {
//   use super::*;
//   #[test]
//   fn permutations_1() {
//     let permutations = permutations_in_memory();
//     assert_eq!(permutations.len(), 120);
//   }
//   #[test]
//   fn test_1() {
//     let program = vec![
//       3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0,
//     ];
//     let mut amps = Amplifiers::new(&program);
//     let actual = amps.test_setting([4, 3, 2, 1, 0]);
//     assert_eq!(actual, 43210);
//   }
//   #[test]
//   fn test_2() {
//     let program = vec![
//       3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99,
//       0, 0,
//     ];
//     let mut amps = Amplifiers::new(&program);
//     let actual = amps.test_setting([0, 1, 2, 3, 4]);
//     assert_eq!(actual, 54321);
//   }
//   #[test]
//   fn test_3() {
//     let program = vec![
//       3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33,
//       31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0,
//     ];
//     let mut amps = Amplifiers::new(&program);
//     let actual = amps.test_setting([1, 0, 4, 3, 2]);
//     assert_eq!(actual, 65210);
//   }
// }
