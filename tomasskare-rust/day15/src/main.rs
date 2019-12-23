use std::cmp;
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

#[derive(Eq, Hash, PartialEq, Debug, Clone)]
struct Point {
    x: i64,
    y: i64,
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

    let mut map = HashMap::<Point, char>::new();

    let mut min = Point { x: 0, y: 0 };
    let mut max = Point { x: 0, y: 0 };
    let mut droid = Point { x: 0, y: 0 };
    let mut oxygen_pos = Point { x: 0, y: 0 };

    let mut dir = 1;

    let mut steps = Vec::<Point>::new();

    let mut program = IntCode::new(v_orig.clone());
    loop {
        let next_point = match dir {
            1 => Point {
                x: droid.x,
                y: droid.y - 1,
            },
            2 => Point {
                x: droid.x,
                y: droid.y + 1,
            },
            3 => Point {
                x: droid.x - 1,
                y: droid.y,
            },
            4 => Point {
                x: droid.x + 1,
                y: droid.y,
            },
            _ => panic!("Shouldn't happen!"),
        };

        program.run(&vec![dir]);
        if program.halted {
            break;
        }
        let status = program.output;

        match status {
            0 => {
                map.insert(next_point.clone(), '#');
                dir = match dir {
                    1 => 4,
                    2 => 3,
                    3 => 1,
                    4 => 2,
                    _ => panic!("Shouldn't happen"),
                }
            }
            1 => {
                map.insert(droid.clone(), '.');

                let mut reset = false;
                if steps.len() > 0 {
                    let mut i = steps.len() - 1;
                    loop {
                        if steps[i] == next_point {
                            steps.resize(i, Point { x: 0, y: 0 });
                            reset = true;
                            break;
                        }
                        if i == 0 {
                            break;
                        }
                        i -= 1;
                    }
                }
                if !reset {
                    steps.push(droid.clone());
                }

                droid = next_point.clone();
                if droid.x == 0 && droid.y == 0 {
                    break;
                }

                dir = match dir {
                    1 => 3,
                    2 => 4,
                    3 => 2,
                    4 => 1,
                    _ => panic!("Shouldn't happen"),
                }
            }
            2 => {
                map.insert(droid.clone(), '.');
                oxygen_pos = next_point.clone();
                droid = next_point.clone();
                steps.push(droid.clone());
                println!("Part 1: {}", steps.len());

                // Keep going for part 2
                droid = next_point.clone();
                dir = match dir {
                    1 => 3,
                    2 => 4,
                    3 => 2,
                    4 => 1,
                    _ => panic!("Shouldn't happen"),
                }
            }
            x => panic!("Unknown status {}", x),
        }

        min.x = cmp::min(min.x, next_point.x);
        max.x = cmp::max(max.x, next_point.x);
        min.y = cmp::min(min.y, next_point.y);
        max.y = cmp::max(max.y, next_point.y);
    }

    let mut minutes = 0;
    let mut oxygens = Vec::<Point>::new();
    map.insert(oxygen_pos.clone(), 'O');
    oxygens.push(oxygen_pos.clone());
    loop {
        let ochecks = oxygens.clone();
        oxygens = Vec::<Point>::new();
        for opos in ochecks {
            check_add_oxygen(
                &mut map,
                &mut oxygens,
                Point {
                    x: opos.x,
                    y: opos.y - 1,
                },
            );
            check_add_oxygen(
                &mut map,
                &mut oxygens,
                Point {
                    x: opos.x,
                    y: opos.y + 1,
                },
            );
            check_add_oxygen(
                &mut map,
                &mut oxygens,
                Point {
                    x: opos.x - 1,
                    y: opos.y,
                },
            );
            check_add_oxygen(
                &mut map,
                &mut oxygens,
                Point {
                    x: opos.x + 1,
                    y: opos.y,
                },
            );
        }

        if oxygens.len() == 0 {
            break;
        }
        minutes += 1;
    }

    println!("Part 2: {}", minutes);
}

fn check_add_oxygen(map: &mut HashMap<Point, char>, oxygens: &mut Vec<Point>, pos: Point) {
    if let Some('.') = map.get(&pos) {
        map.insert(pos.clone(), 'O');
        oxygens.push(pos);
    }
}
