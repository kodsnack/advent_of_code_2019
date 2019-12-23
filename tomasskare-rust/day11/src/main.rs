use std::cmp::{max, min};
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

struct IntCode {
    program: Vec<i64>,
    output: i64,
    pc: usize,
    rbo: usize,
    halted: bool,
}

impl IntCode {
    fn new(program: Vec<i64>) -> IntCode {
        IntCode {
            program,
            output: 0,
            pc: 0,
            rbo: 0,
            halted: false,
        }
    }

    fn get_address(&mut self, pm: i64, pcoffset: usize) -> usize {
        let i = self.program[self.pc + pcoffset];
        match pm {
            0 | 1 => {
                self.expand(i as usize);
                i as usize
            }
            2 => {
                let off = if i < 0 {
                    self.rbo - i.abs() as usize
                } else {
                    self.rbo + i as usize
                };
                self.expand(off);
                off
            }
            _ => panic!("Unsupported pm {}", pm),
        }
    }

    fn get_value(&mut self, pm: i64, pcoffset: usize) -> i64 {
        let i = self.program[self.pc + pcoffset];
        match pm {
            0 => {
                self.expand(i as usize);
                self.program[i as usize]
            }
            1 => i,
            2 => {
                let off = if i < 0 {
                    self.rbo - i.abs() as usize
                } else {
                    self.rbo + i as usize
                };
                self.expand(off);
                self.program[off]
            }
            _ => panic!("Unsupported pm {}", pm),
        }
    }

    fn expand(&mut self, offset: usize) {
        if offset + 1 > self.program.len() {
            self.program.resize_with(offset + 1, || 0);
        }
    }

    fn run(&mut self, inputs: &Vec<i64>) {
        let mut input_idx: usize = 0;
        loop {
            let pm1 = (self.program[self.pc] / 100) % 10;
            let pm2 = (self.program[self.pc] / 1000) % 10;
            let pm3 = (self.program[self.pc] / 10000) % 10;
            match self.program[self.pc] % 100 {
                // Add
                1 => {
                    let v1 = self.get_value(pm1, 1);
                    let v2 = self.get_value(pm2, 2);
                    let a3 = self.get_address(pm3, 3);
                    self.program[a3 as usize] = v1 + v2;
                    self.pc += 4;
                }
                // Multiply
                2 => {
                    let v1 = self.get_value(pm1, 1);
                    let v2 = self.get_value(pm2, 2);
                    let a3 = self.get_address(pm3, 3);
                    self.program[a3 as usize] = v1 * v2;
                    self.pc += 4;
                }
                // Input
                3 => {
                    let a1 = self.get_address(pm1, 1);
                    self.program[a1 as usize] = inputs[input_idx];
                    input_idx += 1;
                    self.pc += 2;
                }
                // Output
                4 => {
                    let v1 = self.get_value(pm1, 1);
                    self.output = v1;
                    self.pc += 2;
                    return;
                }
                // Jump if true
                5 => {
                    let v1 = self.get_value(pm1, 1);
                    let v2 = self.get_value(pm2, 2);
                    if v1 != 0 {
                        self.expand(v2 as usize);
                        self.pc = v2 as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                // Jump if false
                6 => {
                    let v1 = self.get_value(pm1, 1);
                    let v2 = self.get_value(pm2, 2);
                    if v1 == 0 {
                        self.expand(v2 as usize);
                        self.pc = v2 as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                // Less than
                7 => {
                    let v1 = self.get_value(pm1, 1);
                    let v2 = self.get_value(pm2, 2);
                    let a3 = self.get_address(pm3, 3);
                    self.program[a3 as usize] = if v1 < v2 { 1 } else { 0 };
                    self.pc += 4;
                }
                // Equal to
                8 => {
                    let v1 = self.get_value(pm1, 1);
                    let v2 = self.get_value(pm2, 2);
                    let a3 = self.get_address(pm3, 3);
                    self.program[a3 as usize] = if v1 == v2 { 1 } else { 0 };
                    self.pc += 4;
                }
                // Update relative base offset
                9 => {
                    let v1 = self.get_value(pm1, 1);
                    if v1 < 0 {
                        self.rbo -= v1.abs() as usize;
                    } else {
                        self.rbo += v1 as usize;
                    }
                    self.pc += 2;
                }
                99 => {
                    self.halted = true;
                    return;
                }
                x => panic!("Unexpected opcode {}", x),
            }
        }
    }
}

#[derive(Eq, Hash, PartialEq, Debug)]
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    let filename = "input.txt";

    let fd = File::open(filename).expect(&format!("Failure opening {}", filename));
    let buf = BufReader::new(fd);
    let mut v_orig = Vec::new();
    buf.lines().for_each(|line| {
        line.unwrap().split(',').for_each(|numstr| {
            let num = numstr.parse::<i64>().unwrap();
            v_orig.push(num);
        });
    });

    let mut map = HashMap::<Point, i64>::new();
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut dx: i32 = 0;
    let mut dy: i32 = -1;

    let mut program = IntCode::new(v_orig.clone());
    loop {
        let currcol = map.get(&Point { x, y }).unwrap_or(&0);
        program.run(&vec![*currcol]);
        if program.halted {
            break;
        }
        let newcol = program.output;
        program.run(&vec![]);
        let newdir = program.output;

        map.insert(Point { x, y }, newcol);
        if newdir == 0 && dx == 0 {
            dx = dy;
            dy = 0;
        } else if newdir == 0 && dy == 0 {
            dy = -dx;
            dx = 0;
        } else if newdir == 1 && dx == 0 {
            dx = -dy;
            dy = 0;
        } else if newdir == 1 && dy == 0 {
            dy = dx;
            dx = 0;
        }

        x += dx;
        y += dy;
    }

    println!("Part 1: {}", map.len());

    let mut map = HashMap::<Point, i64>::new();
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut dx: i32 = 0;
    let mut dy: i32 = -1;
    let mut min_x: i32 = 0;
    let mut max_x: i32 = 0;
    let mut min_y: i32 = 0;
    let mut max_y: i32 = 0;

    let mut program = IntCode::new(v_orig.clone());
    let mut first_panel = true;
    loop {
        let currcol = if first_panel {
            &1
        } else {
            map.get(&Point { x, y }).unwrap_or(&0)
        };
        first_panel = false;
        program.run(&vec![*currcol]);
        if program.halted {
            break;
        }
        let newcol = program.output;
        program.run(&vec![]);
        let newdir = program.output;

        map.insert(Point { x, y }, newcol);
        if newdir == 0 && dx == 0 {
            dx = dy;
            dy = 0;
        } else if newdir == 0 && dy == 0 {
            dy = -dx;
            dx = 0;
        } else if newdir == 1 && dx == 0 {
            dx = -dy;
            dy = 0;
        } else if newdir == 1 && dy == 0 {
            dy = dx;
            dx = 0;
        }

        x += dx;
        y += dy;

        min_x = min(min_x, x);
        max_x = max(max_x, x);
        min_y = min(min_y, y);
        max_y = max(max_y, y);
    }

    println!("Part 2:");
    for y in min_y..=max_y {
        for x in min_x..=max_x {
            let col = map.get(&Point { x, y }).unwrap_or(&0);
            print!("{}", if *col == 1 { "#" } else { " " });
        }
        println!("");
    }
}
