use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

#[derive(Hash, Eq, PartialEq, Debug)]
struct Coord {
    x: i32,
    y: i32,
}

fn calc_close_dist(currclosest: i32, x: i32, y: i32) -> i32 {
    let dist = x.abs() + y.abs();
    if currclosest == -1 || dist < currclosest {
        dist
    } else {
        currclosest
    }
}

fn calc_close_steps(currclosest: u32, steps1: u32, steps2: u32) -> u32 {
    let stepsum = steps1 + steps2;
    if currclosest == 0 || stepsum < currclosest {
        stepsum
    } else {
        currclosest
    }
}

fn main() {
    let filename = "input.txt";

    let fd = File::open(filename).expect(&format!("Failure opening {}", filename));
    let buf = BufReader::new(fd);
    let mut lines = buf.lines();
    let bufline1 = lines.next().unwrap().unwrap();
    let bufline2 = lines.next().unwrap().unwrap();

    let mut map = HashMap::<Coord, u32>::new();
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut steps: u32 = 0;

    bufline1.split(',').for_each(|instr| {
        let (dir, lenstr) = instr.split_at(1);
        let len = lenstr.parse::<i32>().unwrap();
        let (dx, dy) = match dir {
            "U" => (0, 1),
            "D" => (0, -1),
            "R" => (1, 0),
            "L" => (-1, 0),
            unknown => panic!("Unknown instruction '{}'", unknown),
        };
        for _ in 0..len {
            steps += 1;
            x += dx;
            y += dy;
            if !map.contains_key(&Coord { x, y }) {
                map.insert(Coord { x, y }, steps);
            }
        }
    });

    let mut closestdist = -1;
    let mut closeststeps = 0;
    x = 0;
    y = 0;
    steps = 0;

    bufline2.split(',').for_each(|instr| {
        let (dir, lenstr) = instr.split_at(1);
        let len = lenstr.parse::<i32>().unwrap();
        let (dx, dy) = match dir {
            "U" => (0, 1),
            "D" => (0, -1),
            "R" => (1, 0),
            "L" => (-1, 0),
            unknown => panic!("Unknown instruction '{}'", unknown),
        };
        for _ in 0..len {
            steps += 1;
            x += dx;
            y += dy;
            if map.contains_key(&Coord { x, y }) {
                closestdist = calc_close_dist(closestdist, x, y);
                closeststeps =
                    calc_close_steps(closeststeps, *map.get(&Coord { x, y }).unwrap(), steps);
            }
        }
    });

    println!("Part 1: {}", closestdist);
    println!("Part 1: {}", closeststeps);
}
