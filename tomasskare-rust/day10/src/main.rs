use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
struct Point {
    x: i32,
    y: i32,
}

fn calc_gcd(m: i32, n: i32) -> i32 {
    if m == 0 {
        n.abs()
    } else {
        calc_gcd(n % m, m)
    }
}

fn main() {
    let filename = "input.txt";

    let fd = File::open(filename).expect(&format!("Failure opening {}", filename));
    let buf = BufReader::new(fd);

    let mut map = HashSet::<Point>::new();

    let mut height = 0;
    let mut width = 0;
    let mut first_line = true;
    buf.lines().for_each(|line| {
        let mut x = 0;
        line.unwrap().chars().for_each(|chr| {
            if chr == '#' {
                if first_line {
                    width += 1;
                }
                map.insert(Point { x, y: height });
            } else if chr == '.' {
                if first_line {
                    width += 1;
                }
            }
            x += 1;
        });
        height += 1;
        first_line = false;
    });

    let mut highest_num_hit = 0;
    let mut highest_p = Point { x: 0, y: 0 };

    for p1 in &map {
        let mut checked = HashSet::<Point>::new();
        let mut num_hit = 0;

        for p2 in &map {
            if p1 == p2 {
                continue;
            }

            let mut dx = p2.x - p1.x;
            let mut dy = p2.y - p1.y;
            let gcd = calc_gcd(dx, dy);
            dx /= gcd;
            dy /= gcd;

            if checked.contains(&Point { x: dx, y: dy }) {
                continue;
            }

            checked.insert(Point { x: dx, y: dy });

            let mut x = p1.x + dx;
            let mut y = p1.y + dy;
            while x >= 0 && x < width && y >= 0 && y < height {
                if map.contains(&Point { x, y }) {
                    num_hit += 1;
                    break;
                }
                x += dx;
                y += dy;
            }
        }

        if num_hit > highest_num_hit {
            highest_num_hit = num_hit;
            highest_p = p1.clone();
        }
    }

    println!("Part 1: {}", highest_num_hit);

    let mut radians = Vec::<u32>::new();
    let mut raddxdy = HashMap::<u32, Point>::new();
    for p2 in &map {
        if highest_p == *p2 {
            continue;
        }

        let mut dx = p2.x - highest_p.x;
        let mut dy = p2.y - highest_p.y;
        let gcd = calc_gcd(dx, dy);
        dx /= gcd;
        dy /= gcd;

        let fdx: f64 = dx as f64;
        let fdy: f64 = dy as f64;
        let rad1 = fdy.atan2(fdx);
        let rad = if rad1 < 0f64 {
            std::f64::consts::PI * 2f64 + rad1
        } else {
            rad1
        };

        let radrot = rad - 3f64 * std::f64::consts::PI / 2f64;
        let rad = if radrot < 0f64 {
            radrot + std::f64::consts::PI * 2f64
        } else {
            radrot
        };

        let radu = (rad * 1000f64) as u32;
        radians.push(radu);
        raddxdy.insert(radu, Point { x: dx, y: dy });
    }

    radians.sort();
    radians.dedup();

    let mut i = 0;
    'outer: loop {
        for radu in &radians {
            if let Some(Point { x: dx, y: dy }) = raddxdy.get(&radu) {
                let mut x = highest_p.x + dx;
                let mut y = highest_p.y + dy;
                while x >= 0 && x < width && y >= 0 && y < height {
                    if map.remove(&Point { x, y }) {
                        i += 1;
                        if i == 200 {
                            println!("Part 2: {}", x * 100 + y);
                            break 'outer;
                        }
                        break;
                    }
                    x += dx;
                    y += dy;
                }
            } else {
                panic!("Shouldn't happen, radu {} not found in raddxdy map", radu);
            }
        }
    }
}
