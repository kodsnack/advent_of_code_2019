pub mod a {
  pub fn run() -> String {
    let orbits = super::Orbits::from_strings(
      &loader::load_as("./day/06/input.csv", |l| l).collect::<Vec<String>>(),
    );
    format!("{}", orbits.count())
  }
}
pub mod b {
  pub fn run() -> String {
    let orbits = super::Orbits::from_strings(
      &loader::load_as("./day/06/input.csv", |l| l).collect::<Vec<String>>(),
    );
    format!(
      "{}",
      orbits
        .transferes_between(&"YOU".to_owned(), &"SAN".to_owned())
        .unwrap()
    )
  }
}

pub struct Orbits {
  orbit_count: usize,
  object: String,
  orbits: Vec<Orbits>,
}

impl Orbits {
  fn orbit_tuple(line: &str) -> (String, String) {
    let parts: Vec<&str> = line.split(')').collect();
    if parts.len() != 2 {
      panic!("bad length of input");
    }
    (parts[0].to_owned(), parts[1].to_owned())
  }
  fn child_orbits(&self) -> usize {
    self.orbits.iter().map(|o| o.child_orbits()).sum::<usize>() + self.orbit_count
  }
  pub fn count(&self) -> usize {
    self.child_orbits()
  }
  pub fn print(&self) -> String {
    format!("{}:{}", self.object, self.count())
  }
  fn get_descendants(object: &str, orbit_count: usize, data: &[(String, String)]) -> Orbits {
    Orbits {
      object: object.to_string(),
      orbit_count,
      orbits: data
        .iter()
        .filter_map(|(a, b)| {
          if a == object {
            Some(Orbits::get_descendants(b, orbit_count + 1, data))
          } else {
            None
          }
        })
        .collect(),
    }
  }
  pub fn min_path(&self, object: &str) -> Option<usize> {
    if self.object == *object {
      return Some(0);
    }
    self
      .orbits
      .iter()
      .filter_map(|o| o.min_path(object).map(|v| v + 1))
      .min()
  }
  pub fn transferes_between(&self, start: &str, stop: &str) -> Option<usize> {
    match self
      .orbits
      .iter()
      .filter_map(|o| o.transferes_between(start, stop))
      .min()
    {
      None => {
        if let Some(start_path) = self.min_path(start) {
          if let Some(stop_path) = self.min_path(stop) {
            return Some(start_path + stop_path - 2);
          }
        }
        None
      }
      c => c,
    }
  }
  pub fn from_strings(data: &[String]) -> Orbits {
    let mut entries: Vec<(String, String)> = data.iter().map(|l| Orbits::orbit_tuple(l)).collect();
    entries.sort();
    Orbits::get_descendants("COM", 0, entries.as_slice())
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn truth_a() {
    // 151345
  }
  #[test]
  fn parse_test() {
    let actual: Vec<(String, String)> = ["a)b"].iter().map(|l| Orbits::orbit_tuple(l)).collect();
    let expected = vec![("a".to_owned(), "b".to_owned())];
    assert_eq!(actual, expected);
  }
  #[test]
  fn total_count() {
    let data: Vec<String> = "COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L"
      .split(',')
      .map(|e| e.to_owned())
      .collect();
    let orbits = Orbits::from_strings(&data);
    assert_eq!(orbits.count(), 42);
  }
  #[test]
  fn transfere_count() {
    let data: Vec<String> = "COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L,K)YOU,I)SAN"
      .split(',')
      .map(|e| e.to_owned())
      .collect();
    let orbits = Orbits::from_strings(&data);
    assert_eq!(
      orbits
        .transferes_between(&"YOU".to_owned(), &"SAN".to_owned())
        .unwrap(),
      4
    );
  }
  #[test]
  fn step_count_1() {
    let data: Vec<String> = "COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L,K)YOU,I)SAN"
      .split(',')
      .map(|e| e.to_owned())
      .collect();
    let orbits = Orbits::from_strings(&data);
    assert_eq!(orbits.min_path(&"YOU".to_owned()).unwrap(), 7);
  }
  #[test]
  fn step_count_2() {
    let data: Vec<String> = "COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L,K)YOU,I)SAN"
      .split(',')
      .map(|e| e.to_owned())
      .collect();
    let orbits = Orbits::from_strings(&data);
    assert_eq!(orbits.min_path(&"NOT_IN_DATA".to_owned()), None);
  }
  #[test]
  fn step_count_3() {
    let data: Vec<String> = "COM)B,B)C,C)D,D)E,E)F,B)G,G)H,D)I,E)J,J)K,K)L,K)YOU,I)SAN"
      .split(',')
      .map(|e| e.to_owned())
      .collect();
    let orbits = Orbits::from_strings(&data);
    assert_eq!(orbits.min_path(&"SAN".to_owned()).unwrap(), 5);
  }
}
