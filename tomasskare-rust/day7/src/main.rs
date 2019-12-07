use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

struct IntCode {
    program: Vec<i32>,
    output: i32,
    pc: usize,
    halted: bool,
}

impl IntCode {
    fn new(program: Vec<i32>) -> IntCode {
        IntCode {
            program,
            output: 0,
            pc: 0,
            halted: false,
        }
    }

    fn run(&mut self, inputs: &Vec<i32>) {
        let mut input_idx: usize = 0;
        loop {
            let pm1 = (self.program[self.pc] / 100) % 10;
            let pm2 = (self.program[self.pc] / 1000) % 10;
            let _pm3 = (self.program[self.pc] / 10000) % 10;
            match self.program[self.pc] % 100 {
                // Add
                1 => {
                    let i1 = self.program[self.pc + 1];
                    let i2 = self.program[self.pc + 2];
                    let i3 = self.program[self.pc + 3];
                    let v1 = if pm1 != 0 {
                        i1
                    } else {
                        self.program[i1 as usize]
                    };
                    let v2 = if pm2 != 0 {
                        i2
                    } else {
                        self.program[i2 as usize]
                    };
                    self.program[i3 as usize] = v1 + v2;
                    self.pc += 4;
                }
                // Multiply
                2 => {
                    let i1 = self.program[self.pc + 1];
                    let i2 = self.program[self.pc + 2];
                    let i3 = self.program[self.pc + 3];
                    let v1 = if pm1 != 0 {
                        i1
                    } else {
                        self.program[i1 as usize]
                    };
                    let v2 = if pm2 != 0 {
                        i2
                    } else {
                        self.program[i2 as usize]
                    };
                    self.program[i3 as usize] = v1 * v2;
                    self.pc += 4;
                }
                // Input
                3 => {
                    let i1 = self.program[self.pc + 1];
                    self.program[i1 as usize] = inputs[input_idx];
                    input_idx += 1;
                    self.pc += 2;
                }
                // Output
                4 => {
                    let i1 = self.program[self.pc + 1];
                    let v1 = if pm1 != 0 {
                        i1
                    } else {
                        self.program[i1 as usize]
                    };
                    self.output = v1;
                    self.pc += 2;
                    return;
                }
                // Jump if true
                5 => {
                    let i1 = self.program[self.pc + 1];
                    let i2 = self.program[self.pc + 2];
                    let v1 = if pm1 != 0 {
                        i1
                    } else {
                        self.program[i1 as usize]
                    };
                    let v2 = if pm2 != 0 {
                        i2
                    } else {
                        self.program[i2 as usize]
                    };
                    if v1 != 0 {
                        self.pc = v2 as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                // Jump if false
                6 => {
                    let i1 = self.program[self.pc + 1];
                    let i2 = self.program[self.pc + 2];
                    let v1 = if pm1 != 0 {
                        i1
                    } else {
                        self.program[i1 as usize]
                    };
                    let v2 = if pm2 != 0 {
                        i2
                    } else {
                        self.program[i2 as usize]
                    };
                    if v1 == 0 {
                        self.pc = v2 as usize;
                    } else {
                        self.pc += 3;
                    }
                }
                // Less than
                7 => {
                    let i1 = self.program[self.pc + 1];
                    let i2 = self.program[self.pc + 2];
                    let i3 = self.program[self.pc + 3];
                    let v1 = if pm1 != 0 {
                        i1
                    } else {
                        self.program[i1 as usize]
                    };
                    let v2 = if pm2 != 0 {
                        i2
                    } else {
                        self.program[i2 as usize]
                    };
                    self.program[i3 as usize] = if v1 < v2 { 1 } else { 0 };
                    self.pc += 4;
                }
                // Equal to
                8 => {
                    let i1 = self.program[self.pc + 1];
                    let i2 = self.program[self.pc + 2];
                    let i3 = self.program[self.pc + 3];
                    let v1 = if pm1 != 0 {
                        i1
                    } else {
                        self.program[i1 as usize]
                    };
                    let v2 = if pm2 != 0 {
                        i2
                    } else {
                        self.program[i2 as usize]
                    };
                    self.program[i3 as usize] = if v1 == v2 { 1 } else { 0 };
                    self.pc += 4;
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

    let mut highest_thrust: i32 = 0;
    let mut programs = Vec::<IntCode>::new();

    for _ in 0..5 {
        programs.push(IntCode::new(v_orig.clone()));
    }

    for p1 in 0..5 {
        for p2 in 0..5 {
            if p2 == p1 {
                continue;
            }
            for p3 in 0..5 {
                if p3 == p1 || p3 == p2 {
                    continue;
                }
                for p4 in 0..5 {
                    if p4 == p1 || p4 == p2 || p4 == p3 {
                        continue;
                    }
                    for p5 in 0..5 {
                        if p5 == p1 || p5 == p2 || p5 == p3 || p5 == p4 {
                            continue;
                        }

                        programs[0] = IntCode::new(v_orig.clone());
                        programs[0].run(&vec![p1, 0]);
                        let output = programs[0].output;

                        programs[1] = IntCode::new(v_orig.clone());
                        programs[1].run(&vec![p2, output]);
                        let output = programs[1].output;

                        programs[2] = IntCode::new(v_orig.clone());
                        programs[2].run(&vec![p3, output]);
                        let output = programs[2].output;

                        programs[3] = IntCode::new(v_orig.clone());
                        programs[3].run(&vec![p4, output]);
                        let output = programs[3].output;

                        programs[4] = IntCode::new(v_orig.clone());
                        programs[4].run(&vec![p5, output]);
                        let output = programs[4].output;

                        if output > highest_thrust {
                            highest_thrust = output;
                        }
                    }
                }
            }
        }
    }

    println!("Part 1: {}", highest_thrust);

    let mut highest_thrust: i32 = 0;

    for p1 in 5..10 {
        for p2 in 5..10 {
            if p2 == p1 {
                continue;
            }
            for p3 in 5..10 {
                if p3 == p1 || p3 == p2 {
                    continue;
                }
                for p4 in 5..10 {
                    if p4 == p1 || p4 == p2 || p4 == p3 {
                        continue;
                    }
                    for p5 in 5..10 {
                        if p5 == p1 || p5 == p2 || p5 == p3 || p5 == p4 {
                            continue;
                        }

                        for i in 0..5 {
                            programs[i] = IntCode::new(v_orig.clone());
                        }

                        let mut p0_input = 0;
                        let mut inputs: [Vec<i32>; 5] =
                            [vec![p1], vec![p2], vec![p3], vec![p4], vec![p5]];

                        loop {
                            inputs[0].push(p0_input);
                            programs[0].run(&inputs[0]);

                            inputs[1].push(programs[0].output);
                            programs[1].run(&inputs[1]);

                            inputs[2].push(programs[1].output);
                            programs[2].run(&inputs[2]);

                            inputs[3].push(programs[2].output);
                            programs[3].run(&inputs[3]);

                            inputs[4].push(programs[3].output);
                            programs[4].run(&inputs[4]);

                            let output = programs[4].output;
                            if output > highest_thrust {
                                highest_thrust = output;
                            }

                            if programs[4].halted {
                                break;
                            } else {
                                inputs = [vec![], vec![], vec![], vec![], vec![]];
                                p0_input = output;
                            }
                        }
                    }
                }
            }
        }
    }

    println!("Part 2: {}", highest_thrust);
}
