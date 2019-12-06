use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn intcode_run(v: &mut Vec<i32>, input: i32) {
    let mut i: usize = 0;
    loop {
        let pm1 = (v[i] / 100) % 10;
        let pm2 = (v[i] / 1000) % 10;
        let _pm3 = (v[i] / 10000) % 10;
        match v[i] % 100 {
            // Add
            1 => {
                let i1 = v[i + 1];
                let i2 = v[i + 2];
                let i3 = v[i + 3];
                let v1 = if pm1 != 0 { i1 } else { v[i1 as usize] };
                let v2 = if pm2 != 0 { i2 } else { v[i2 as usize] };
                v[i3 as usize] = v1 + v2;
                i += 4;
            }
            // Multiply
            2 => {
                let i1 = v[i + 1];
                let i2 = v[i + 2];
                let i3 = v[i + 3];
                let v1 = if pm1 != 0 { i1 } else { v[i1 as usize] };
                let v2 = if pm2 != 0 { i2 } else { v[i2 as usize] };
                v[i3 as usize] = v1 * v2;
                i += 4;
            }
            // Input
            3 => {
                let i1 = v[i + 1];
                v[i1 as usize] = input;
                i += 2;
            }
            // Output
            4 => {
                let i1 = v[i + 1];
                let v1 = if pm1 != 0 { i1 } else { v[i1 as usize] };
                println!("output: {}", v1);
                i += 2;
            }
            // Jump if true
            5 => {
                let i1 = v[i + 1];
                let i2 = v[i + 2];
                let v1 = if pm1 != 0 { i1 } else { v[i1 as usize] };
                let v2 = if pm2 != 0 { i2 } else { v[i2 as usize] };
                if v1 != 0 {
                    i = v2 as usize;
                } else {
                    i += 3;
                }
            }
            // Jump if false
            6 => {
                let i1 = v[i + 1];
                let i2 = v[i + 2];
                let v1 = if pm1 != 0 { i1 } else { v[i1 as usize] };
                let v2 = if pm2 != 0 { i2 } else { v[i2 as usize] };
                if v1 == 0 {
                    i = v2 as usize;
                } else {
                    i += 3;
                }
            }
            // Less than
            7 => {
                let i1 = v[i + 1];
                let i2 = v[i + 2];
                let i3 = v[i + 3];
                let v1 = if pm1 != 0 { i1 } else { v[i1 as usize] };
                let v2 = if pm2 != 0 { i2 } else { v[i2 as usize] };
                v[i3 as usize] = if v1 < v2 { 1 } else { 0 };
                i += 4;
            }
            // Equal to
            8 => {
                let i1 = v[i + 1];
                let i2 = v[i + 2];
                let i3 = v[i + 3];
                let v1 = if pm1 != 0 { i1 } else { v[i1 as usize] };
                let v2 = if pm2 != 0 { i2 } else { v[i2 as usize] };
                v[i3 as usize] = if v1 == v2 { 1 } else { 0 };
                i += 4;
            }
            99 => break,
            x => panic!("Unexpected opcode {}", x),
        }
    }
}

fn main() {
    let filename = "input.txt";

    let fd = File::open(filename).expect(&format!("Failure opening {}", filename));
    let buf = BufReader::new(fd);
    let mut v_orig = Vec::new();
    buf.lines().for_each(|line| {
        line.unwrap().split(',').for_each(|numstr| {
            let num = numstr.parse::<i32>().unwrap();
            v_orig.push(num);
        });
    });

    let mut v = v_orig.clone();
    intcode_run(&mut v, 1);

    let mut v = v_orig.clone();
    intcode_run(&mut v, 5);
}
