use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn fuelneed(mass: i32) -> i32 {
    let fuel = mass / 3 - 2;

    if fuel <= 0 {
        0
    } else {
        fuel + fuelneed(fuel)
    }
}

fn main() {
    let filename = "input.txt";

    let fd = File::open(filename).expect(&format!("Failure opening {}", filename));
    let buf = BufReader::new(fd);
    let mut sum = 0;
    buf.lines().for_each(|line| {
        let num = line.unwrap().parse::<i32>().unwrap();
        sum += num / 3 - 2;
    });

    println!("Part 1: {}", sum);

    let fd = File::open(filename).expect(&format!("Failure opening {}", filename));
    let buf = BufReader::new(fd);
    let mut sum = 0;
    buf.lines().for_each(|line| {
        let num = line.unwrap().parse::<i32>().unwrap();
        sum += fuelneed(num);
    });

    println!("Part 2: {}", sum);
}
