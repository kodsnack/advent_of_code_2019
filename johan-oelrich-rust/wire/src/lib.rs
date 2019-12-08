use std::collections::{HashMap, HashSet};
extern crate loader;
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug, PartialOrd, Ord)]
struct Point(i64, i64);

impl Point {
  #[cfg(test)]
  fn new(x: i64, y: i64) -> Point {
    Point(x, y)
  }
  fn zero() -> Point {
    Point(0, 0)
  }
  fn visited_points(&self, step: usize, movement: &Move) -> Vec<(usize, Point)> {
    Point::from_move(movement)
      .iter()
      .enumerate()
      .map(|(idx, p)| (step + idx, self.add(p)))
      .collect()
  }
  fn right(v: i64) -> Point {
    Point(v, 0)
  }
  fn left(v: i64) -> Point {
    Point(-v, 0)
  }
  fn up(v: i64) -> Point {
    Point(0, v)
  }
  fn down(v: i64) -> Point {
    Point(0, -v)
  }
  fn from_move(movement: &Move) -> Vec<Point> {
    match movement {
      Move::Right(val) => (0..=*val).map(Point::right).collect(),
      Move::Left(val) => (0..=*val).map(Point::left).collect(),
      Move::Up(val) => (0..=*val).map(Point::up).collect(),
      Move::Down(val) => (0..=*val).map(Point::down).collect(),
    }
  }
  fn add(&self, point: &Point) -> Point {
    Point(self.0 + point.0, self.1 + point.1)
  }
}
fn common_points(lhs: &[(usize, Point)], rhs: &[(usize, Point)]) -> Vec<(usize, Point)> {
  lhs
    .iter()
    .filter_map(|(s, p)| match rhs.iter().find(|(_rs, rp)| p == rp) {
      None => None,
      Some((rs, _rp)) => Some((s + rs, *p)),
    })
    .collect()
}

pub fn solve_day03b() -> i64 {
  fewest_steps(load_wires()).unwrap()
}

fn fewest_steps(wires: Vec<Vec<Move>>) -> Option<i64> {
  fewest_steps_slow(wires)
}

//#[cfg(any(test, feature = "slowly"))]
//use rayon::prelude::*;

//#[cfg(any(test, feature = "slowly"))]
//fn fewest_steps_quicker(wires: Vec<Vec<Move>>) -> Option<i64> {
//let mut point_steps: HashMap<Point, (usize, usize)> = HashMap::new();

// wires.par_iter().for_each(|wire| {
//   crate::wire::visited_point_at_move(wire)
//    .par_iter()
// .for_each(|(s, p)| {point_steps.entry(*p).and_modify(|e| if *s < *e { *e = *s}).or_insert(*s);})
//});
//  None
//}

fn fewest_steps_slow(wires: Vec<Vec<Move>>) -> Option<i64> {
  let wire_visited: Vec<Vec<(usize, Point)>> = wires
    .iter()
    .map(|moves| visited_point_at_move(moves))
    .collect();
  assert_eq!(wire_visited.len(), 2);
  let common = common_points(&wire_visited[0], &wire_visited[1]);
  common
    .iter()
    .filter_map(|(s, _p)| {
      if *s > 0 {
        return Some(*s as i64);
      };
      None
    })
    .min()
}

pub fn solve_day03a() -> i64 {
  shortest_distance(load_wires()).unwrap()
}

fn visited_point_at_move(wire: &[Move]) -> Vec<(usize, Point)> {
  let mut visited: Vec<(usize, Point)> = Vec::new();
  let mut step = 0;
  let mut current = Point::zero();
  for m in wire {
    let mut steps = current.visited_points(step, m);
    if let Some((current_step, point)) = steps.pop() {
      current = point;
      step = current_step;
    }
    visited.append(&mut steps);
  }
  visited.push((step, current));
  visited.dedup();
  visited
}

fn visited_points(moves: &[Move]) -> Vec<Point> {
  let mut points = HashSet::new();
  moves.iter().fold(Point::zero(), |state, movement| {
    let visited = state.visited_points(0, movement);
    let next = visited[visited.len() - 1];
    visited.iter().for_each(|(_s, p)| {
      points.insert(p.clone());
    });
    next.1
  });

  points.iter().cloned().collect()
}

fn intersection_points(wires: Vec<Vec<Move>>) -> Vec<(Point, usize)> {
  let mut visited = HashMap::new();
  wires.iter().enumerate().for_each(|(idx, moves)| {
    visited_points(moves)
      .iter()
      .enumerate()
      .for_each(|(w_idx, point)| {
        visited
          .entry(*point)
          .or_insert_with(Vec::new)
          .push((idx, w_idx));
      })
  });
  visited
    .iter()
    .filter(|(_k, v)| {
      let mut wires_at_pos: Vec<usize> = v.iter().cloned().map(|(i, _)| i).collect();
      wires_at_pos.dedup();
      wires_at_pos.len() == wires.len()
    })
    .map(|(k, v)| (*k, v.iter().fold(0, |t, (_, s)| s + t)))
    .collect()
}
fn shortest_distance(wires: Vec<Vec<Move>>) -> Option<i64> {
  intersection_points(wires)
    .iter()
    .map(|(p, _steps)| p.0.abs() + p.1.abs())
    .filter(|e| *e != 0)
    .min()
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Move {
  Right(i64),
  Left(i64),
  Up(i64),
  Down(i64),
}
#[derive(PartialEq, Eq, Debug)]
enum Error {
  BadString(String),
}
impl Move {
  fn many_from_string(text: &str) -> Result<Vec<Move>, Error> {
    text.split(',').map(Move::from_string).collect()
  }
  fn from_string(text: &str) -> Result<Move, Error> {
    if let Some(direction) = text.chars().nth(0) {
      if let Ok(distance) = text.chars().skip(1).collect::<String>().parse::<i64>() {
        match direction {
          'U' => return Ok(Move::Up(distance)),
          'D' => return Ok(Move::Down(distance)),
          'L' => return Ok(Move::Left(distance)),
          'R' => return Ok(Move::Right(distance)),
          _ => return Err(Error::BadString(text.into())),
        }
      }
    }
    Err(Error::BadString(text.into()))
  }
}

fn load_wires() -> Vec<Vec<Move>> {
  loader::load_as::<Vec<Move>>("./day/03/input.csv", |s| {
    Move::many_from_string(&s).unwrap()
  })
  .collect::<Vec<Vec<Move>>>()
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn parse_move() {
    assert_eq!(Move::from_string("L42").unwrap(), Move::Left(42));
  }
  #[test]
  fn visited_points_up() {
    let point = Point::zero();
    let actual = point.visited_points(0, &Move::Up(3));
    let expected = vec![
      (0, Point::new(0, 0)),
      (1, Point::new(0, 1)),
      (2, Point::new(0, 2)),
      (3, Point::new(0, 3)),
    ];
    assert_eq!(actual, expected);
  }
  #[test]
  fn visited_points() {
    let point = Point::zero();
    let actual = point.visited_points(0, &Move::Down(3));
    let expected = vec![
      (0, Point::new(0, 0)),
      (1, Point::new(0, -1)),
      (2, Point::new(0, -2)),
      (3, Point::new(0, -3)),
    ];
    assert_eq!(actual, expected);
  }
  #[cfg(feature = "slowly")]
  #[test]
  fn visited_1() {
    let expected: HashMap<usize, Point> = [
      (0, Point(0, 0)),
      (1, Point(0, 1)),
      (2, Point(0, 2)),
      (3, Point(1, 2)),
      (4, Point(2, 2)),
      (5, Point(2, 1)),
      (6, Point(1, 1)),
    ]
    .iter()
    .cloned()
    .collect();
    let actual: HashMap<usize, Point> =
      super::visited_point_at_move(&Move::many_from_string("U2,R2,D1,L1").unwrap())
        .iter()
        .cloned()
        .collect();
    assert_eq!(actual, expected);
  }
  #[test]
  fn simple_1() {
    let actual = shortest_distance(vec![
      Move::many_from_string("R8,U5,L5,D3").unwrap(),
      Move::many_from_string("U7,R6,D4,L4").unwrap(),
    ])
    .unwrap();
    assert_eq!(actual, 6);
  }
  #[test]
  fn simple_2() {
    let actual = shortest_distance(vec![
      Move::many_from_string("R75,D30,R83,U83,L12,D49,R71,U7,L72").unwrap(),
      Move::many_from_string("U62,R66,U55,R34,D71,R55,D58,R83").unwrap(),
    ])
    .unwrap();
    assert_eq!(actual, 159);
  }
  #[test]
  fn simple_3() {
    let actual = shortest_distance(vec![
      Move::many_from_string("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51").unwrap(),
      Move::many_from_string("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7").unwrap(),
    ])
    .unwrap();
    assert_eq!(actual, 135);
  }
  #[test]
  fn steps_1() {
    let actual = fewest_steps(vec![
      Move::many_from_string("R8,U5,L5,D3").unwrap(),
      Move::many_from_string("U7,R6,D4,L4").unwrap(),
    ])
    .unwrap();
    assert_eq!(actual, 30);
  }
  #[test]
  fn steps_2() {
    let actual = fewest_steps(vec![
      Move::many_from_string("R75,D30,R83,U83,L12,D49,R71,U7,L72").unwrap(),
      Move::many_from_string("U62,R66,U55,R34,D71,R55,D58,R83").unwrap(),
    ])
    .unwrap();
    assert_eq!(actual, 610);
  }
  #[test]
  fn steps_3() {
    let actual = fewest_steps(vec![
      Move::many_from_string("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51").unwrap(),
      Move::many_from_string("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7").unwrap(),
    ])
    .unwrap();
    assert_eq!(actual, 410);
  }
  #[test]
  fn slow() {
    let actual = fewest_steps_slow(vec![
      Move::many_from_string("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51").unwrap(),
      Move::many_from_string("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7").unwrap(),
    ])
    .unwrap();
    assert_eq!(actual, 410);
  }
  #[test]
  fn day03a() {
    assert_eq!(solve_day03a(), 651);
  }
}
