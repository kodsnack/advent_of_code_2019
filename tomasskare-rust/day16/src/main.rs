use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn main() {
    let filename = "input.txt";

    let fd = File::open(filename).expect(&format!("Failure opening {}", filename));
    let buf = BufReader::new(fd);
    let mut input = Vec::<i32>::new();
    buf.lines().for_each(|line| {
        line.unwrap().chars().for_each(|chr| {
            let num = chr.to_string().parse::<i32>().unwrap();
            input.push(num);
        });
    });

    let base_pattern: Vec<i32> = vec![0, 1, 0, -1];
    let digits = input.len();

    let mut inputdigits = input.clone();

    for _phase in 1..=100 {
        let mut newdigits = Vec::<i32>::new();

        for idigit in 0..digits {
            let mut sum = 0;
            for sdigit in 0..digits {
                let idx = ((sdigit + 1) / (idigit + 1)) % 4;
                sum += match idx {
                    1 => inputdigits[sdigit],
                    3 => -inputdigits[sdigit],
                    _ => 0,
                };
            }
            newdigits.push(sum.abs() % 10);
        }

        inputdigits = newdigits;
    }

    let mut v = "".to_string();
    for i in 0..8 {
        v += &inputdigits[i].to_string();
    }
    println!("Part 1: {}", v);

    /*
        // Part 2 - doesn't work

        let mut inputdigits = input.clone();
        println!("digits={}", digits);

        let mut finaloffset: usize = 0;
        for i in 0..7 {
            finaloffset = finaloffset * 10 + inputdigits[i] as usize;
        }

        println!("finaloffset {}", finaloffset);

        for phase in 1..=100 {
            let mut newdigits = Vec::<i32>::new();

            for idigit in 0..digits {
                //            let looplen = (4 * (idigit + 1)) / (digits % 4);
                let looplen = 0;
                println!("step {} looplen {}", idigit, looplen);

                let mut totsum_guess = 0;
                let mut totsum_rest = 0;

                let mut totsum = 0;
                for rep in 0..10000 {
                    let mut sum = 0;
                    //                print!("step {}: rep {}: ", idigit, rep);
                    for sdigit in 0..digits {
                        let idx = (rep * digits + sdigit + 1) / (idigit + 1);
                        sum += inputdigits[sdigit] * base_pattern[idx % 4];
                        if rep < 50 {
                            //                        print!("{}*{} + ", inputdigits[sdigit], base_pattern[idx % 4]);
                        }
                    }
                    if rep < 50 {
                        //                    println!("= {}", sum);
                    }
                    totsum += sum;

                    if rep <= idigit {
                        totsum_guess += sum;
                    }
                    if rep < 10000 % (idigit + 1) {
                        totsum_rest += sum;
                    }
                }
                totsum_guess = totsum_guess * (10000 / (idigit as i32 + 1)) + totsum_rest;
                println!("totsum {} guess {}", totsum, totsum_guess);
                newdigits.push(totsum.abs() % 10);
            }

            println!("phase {}", phase);

            println!("newdigits {:?}", newdigits);

            inputdigits = newdigits;
        }
    */
}
