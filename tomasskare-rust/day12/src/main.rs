extern crate regex;

use regex::Regex;
use std::cmp;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

#[derive(Eq, Hash, PartialEq, Debug, Clone)]
struct Point {
    x: i64,
    y: i64,
    z: i64,
}

#[derive(Eq, Hash, PartialEq, Debug, Clone)]
struct Moon {
    pos: Point,
    vel: Point,
}

fn calc_gcd3(m: i64, n: i64, o: i64) -> i64 {
    let mut i: i64 = cmp::min(m, cmp::min(n, o));
    while i > 1 {
        if m % i == 0 && n % i == 0 && o % i == 0 {
            return i;
        }
        i -= 1;
    }

    1
}

fn calc_moons(moons: &mut Vec<Moon>) {
    for i in 0..moons.len() {
        for j in i + 1..moons.len() {
            if moons[i].pos.x > moons[j].pos.x {
                moons[i].vel.x -= 1;
                moons[j].vel.x += 1;
            } else if moons[i].pos.x < moons[j].pos.x {
                moons[i].vel.x += 1;
                moons[j].vel.x -= 1;
            }
            if moons[i].pos.y > moons[j].pos.y {
                moons[i].vel.y -= 1;
                moons[j].vel.y += 1;
            } else if moons[i].pos.y < moons[j].pos.y {
                moons[i].vel.y += 1;
                moons[j].vel.y -= 1;
            }
            if moons[i].pos.z > moons[j].pos.z {
                moons[i].vel.z -= 1;
                moons[j].vel.z += 1;
            } else if moons[i].pos.z < moons[j].pos.z {
                moons[i].vel.z += 1;
                moons[j].vel.z -= 1;
            }
        }
    }

    for i in 0..moons.len() {
        moons[i].pos.x += moons[i].vel.x;
        moons[i].pos.y += moons[i].vel.y;
        moons[i].pos.z += moons[i].vel.z;
    }
}

fn main() {
    let filename = "input.txt";

    let fd = File::open(filename).expect(&format!("Failure opening {}", filename));
    let buf = BufReader::new(fd);
    let mut moons_orig = Vec::new();
    let re = Regex::new(r"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>").unwrap();
    buf.lines().for_each(|line| {
        let lineu = line.unwrap();
        let cap = re.captures(&lineu).unwrap();
        moons_orig.push(Moon {
            pos: Point {
                x: cap[1].parse::<i64>().unwrap(),
                y: cap[2].parse::<i64>().unwrap(),
                z: cap[3].parse::<i64>().unwrap(),
            },
            vel: Point { x: 0, y: 0, z: 0 },
        });
    });

    let mut moons = moons_orig.clone();

    for _step in 0..1000 {
        calc_moons(&mut moons);
    }

    let mut sum = 0;
    for i in 0..moons.len() {
        let pot = moons[i].pos.x.abs() + moons[i].pos.y.abs() + moons[i].pos.z.abs();
        let kin = moons[i].vel.x.abs() + moons[i].vel.y.abs() + moons[i].vel.z.abs();
        sum += pot * kin;
    }

    println!("Part 1: {}", sum);

    let mut moons = moons_orig.clone();
    let mut set_x = HashSet::<Vec<i64>>::new();
    let mut set_y = HashSet::<Vec<i64>>::new();
    let mut set_z = HashSet::<Vec<i64>>::new();

    set_x.insert(vec![
        moons[0].pos.x,
        moons[1].pos.x,
        moons[2].pos.x,
        moons[3].pos.x,
        moons[0].vel.x,
        moons[1].vel.x,
        moons[2].vel.x,
        moons[3].vel.x,
    ]);
    set_y.insert(vec![
        moons[0].pos.y,
        moons[1].pos.y,
        moons[2].pos.y,
        moons[3].pos.y,
        moons[0].vel.y,
        moons[1].vel.y,
        moons[2].vel.y,
        moons[3].vel.y,
    ]);
    set_z.insert(vec![
        moons[0].pos.z,
        moons[1].pos.z,
        moons[2].pos.z,
        moons[3].pos.z,
        moons[0].vel.z,
        moons[1].vel.z,
        moons[2].vel.z,
        moons[3].vel.z,
    ]);

    let mut per_x = 0;
    let mut per_y = 0;
    let mut per_z = 0;

    let mut steps = 0;
    loop {
        calc_moons(&mut moons);
        steps += 1;

        let vx = vec![
            moons[0].pos.x,
            moons[1].pos.x,
            moons[2].pos.x,
            moons[3].pos.x,
            moons[0].vel.x,
            moons[1].vel.x,
            moons[2].vel.x,
            moons[3].vel.x,
        ];
        let vy = vec![
            moons[0].pos.y,
            moons[1].pos.y,
            moons[2].pos.y,
            moons[3].pos.y,
            moons[0].vel.y,
            moons[1].vel.y,
            moons[2].vel.y,
            moons[3].vel.y,
        ];
        let vz = vec![
            moons[0].pos.z,
            moons[1].pos.z,
            moons[2].pos.z,
            moons[3].pos.z,
            moons[0].vel.z,
            moons[1].vel.z,
            moons[2].vel.z,
            moons[3].vel.z,
        ];

        if per_x == 0 && set_x.contains(&vx) {
            per_x = steps;
        }
        if per_y == 0 && set_y.contains(&vy) {
            per_y = steps;
        }
        if per_z == 0 && set_z.contains(&vz) {
            per_z = steps;
        }

        if per_x != 0 && per_y != 0 && per_z != 0 {
            break;
        }

        set_x.insert(vx);
        set_y.insert(vy);
        set_z.insert(vz);
    }

    let gcd = calc_gcd3(per_x, per_y, per_z);
    per_x /= gcd;
    per_y /= gcd;
    per_z /= gcd;

    let sum = per_x * per_y * per_z;

    println!("Part 2: {}", sum);
}
