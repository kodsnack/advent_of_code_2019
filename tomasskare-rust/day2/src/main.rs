use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn intcode_run(v: &mut Vec<usize>) {
    let mut i: usize = 0;
    loop {
        match v[i] {
            1 => {
                let i1 = v[i + 1];
                let i2 = v[i + 2];
                let i3 = v[i + 3];
                v[i3] = v[i1] + v[i2];
                i += 4;
            }
            2 => {
                let i1 = v[i + 1];
                let i2 = v[i + 2];
                let i3 = v[i + 3];
                v[i3] = v[i1] * v[i2];
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
            let num = numstr.parse::<usize>().unwrap();
            v_orig.push(num);
        });
    });

    let mut v = v_orig.clone();
    v[1] = 12;
    v[2] = 2;
    intcode_run(&mut v);
    println!("Part 1: {}", v[0]);

    'outer: for noun in 0..100 {
        for verb in 0..100 {
            let mut v = v_orig.clone();
            v[1] = noun;
            v[2] = verb;
            intcode_run(&mut v);
            if v[0] == 19690720 {
                println!("Part 2: {}", 100 * noun + verb);
                break 'outer;
            }
        }
    }
}
