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

#[derive(Eq, Hash, PartialEq, Debug, Clone)]
struct Step {
    rot: char,
    steps: i64,
}

fn steps_to_string(steps: &Vec<Step>) -> String {
    let mut strings = Vec::<String>::new();
    for s in steps {
        strings.push(format!("{},{}", s.rot.to_string(), s.steps.to_string()));
    }
    strings.join(",")
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

    let mut width = 0;
    let mut x = 0;
    let mut y = 0;
    let mut path = HashMap::<Point, char>::new();
    let mut robotpos = Point { x: 0, y: 0 };

    let mut program = IntCode::new(v_orig.clone());
    loop {
        program.run(&vec![]);
        if program.halted {
            break;
        }
        let chr = (program.output as u8) as char;

        let pos = Point { x, y };
        x += 1;
        match chr {
            '#' => {
                path.insert(pos.clone(), chr);
            }
            '^' | 'v' | '<' | '>' => {
                path.insert(pos.clone(), '#');
                robotpos = pos.clone();
            }
            '\n' => {
                if width == 0 {
                    width = x;
                }
                y += 1;
                x = 0;
            }
            _ => {}
        };
    }

    let mut sum = 0;
    for (p, _chr) in path.iter() {
        if path.contains_key(&Point { x: p.x - 1, y: p.y })
            && path.contains_key(&Point { x: p.x + 1, y: p.y })
            && path.contains_key(&Point { x: p.x, y: p.y - 1 })
            && path.contains_key(&Point { x: p.x, y: p.y + 1 })
        {
            sum += p.x * p.y;
        }
    }

    println!("Part 1: {}", sum);

    // Part 2

    let mut all_steps = Vec::<Step>::new();

    let mut x = robotpos.x;
    let mut y = robotpos.y;
    let mut dx = 0;
    let mut dy = -1;

    loop {
        let (dx_l, dy_l) = match (dx, dy) {
            (-1, 0) => (0, 1),
            (0, 1) => (1, 0),
            (1, 0) => (0, -1),
            (0, -1) => (-1, 0),
            _ => panic!("Invalid dx/dy {}/{}", dx, dy),
        };
        let (dx_r, dy_r) = match (dx, dy) {
            (-1, 0) => (0, -1),
            (0, -1) => (1, 0),
            (1, 0) => (0, 1),
            (0, 1) => (-1, 0),
            _ => panic!("Invalid dx/dy {}/{}", dx, dy),
        };

        let (rot, new_dx, new_dy) = if path.contains_key(&Point {
            x: x + dx_l,
            y: y + dy_l,
        }) {
            ('L', dx_l, dy_l)
        } else if path.contains_key(&Point {
            x: x + dx_r,
            y: y + dy_r,
        }) {
            ('R', dx_r, dy_r)
        } else {
            break;
        };

        dx = new_dx;
        dy = new_dy;

        let mut steps = 0;
        while path.contains_key(&Point {
            x: x + dx,
            y: y + dy,
        }) {
            x += dx;
            y += dy;
            steps += 1;
        }

        all_steps.push(Step { rot, steps });
    }

    let mut movements = Vec::<String>::new();

    let mut first_movement_max_size = 20;
    let mut steps = all_steps.clone();
    let mut found_any = true;
    while found_any {
        found_any = false;
        let len = steps.len();
        let steps_string = steps_to_string(&steps);
        if steps_string.len() <= 20 {
            break;
        }
        let mut slice_size = len / 2;
        'outer: while slice_size >= 2 {
            let mut slice_start = 0;
            while slice_start < len - slice_size {
                let mut found = Vec::<usize>::new();
                let slice = &steps[slice_start..slice_start + slice_size];
                let slice_string = steps_to_string(&slice.to_vec());
                if slice_string.len() > 20 {
                    break;
                }
                if movements.len() == 0 && slice_string.len() > first_movement_max_size {
                    break;
                }
                let mut cmpoff = slice_start + slice_size;
                while cmpoff <= len - slice_size {
                    let slice2 = &steps[cmpoff..cmpoff + slice_size];
                    if slice == slice2 {
                        found.push(cmpoff);
                        cmpoff += slice_size;
                        continue;
                    }
                    cmpoff += 1;
                }
                if found.len() > 1 {
                    movements.push(steps_to_string(&slice.to_vec()));
                    found.reverse();
                    for f in found {
                        for _ in 0..slice_size {
                            steps.remove(f);
                        }
                    }
                    for _ in 0..slice_size {
                        steps.remove(slice_start);
                    }

                    if movements.len() >= 3 && steps.len() > 0 {
                        first_movement_max_size =
                            cmp::min(first_movement_max_size, movements[0].len() - 1);
                        movements = Vec::<String>::new();
                        steps = all_steps.clone();
                    }
                    found_any = true;
                    break 'outer;
                }
                slice_start += 1;
            }
            slice_size -= 1;
        }
        if !found_any && steps_to_string(&steps).len() > 20 {
            first_movement_max_size = cmp::min(first_movement_max_size, movements[0].len() - 1);
            movements = Vec::<String>::new();
            steps = all_steps.clone();
            found_any = true;
        }
    }
    if steps.len() > 0 {
        movements.push(steps_to_string(&steps));
    }

    let mut steps_string = steps_to_string(&all_steps);
    for moveidx in 0..movements.len() {
        let movechr = ["A", "B", "C"];
        steps_string = steps_string.replacen(&movements[moveidx], &movechr[moveidx], 1000);
    }

    let mut program = IntCode::new(v_orig.clone());
    program.program[0] = 2;
    let mut line = "".to_string();
    let mut input = vec![];
    loop {
        program.run(&input);
        if program.halted {
            break;
        }
        input = vec![];
        let chr = (program.output as u8) as char;
        if chr == '\n' {
            match line.as_str() {
                "Main:" => {
                    input = Vec::<i64>::new();
                    for b in steps_string.as_bytes() {
                        input.push(*b as i64);
                    }
                    input.push(10);
                }
                "Function A:" => {
                    input = Vec::<i64>::new();
                    for b in movements[0].as_bytes() {
                        input.push(*b as i64);
                    }
                    input.push(10);
                }
                "Function B:" => {
                    input = Vec::<i64>::new();
                    for b in movements[1].as_bytes() {
                        input.push(*b as i64);
                    }
                    input.push(10);
                }
                "Function C:" => {
                    input = Vec::<i64>::new();
                    for b in movements[2].as_bytes() {
                        input.push(*b as i64);
                    }
                    input.push(10);
                }
                "Continuous video feed?" => {
                    input.push(110);
                    input.push(10);
                }
                _ => {}
            }
            line = "".to_string();
        } else {
            line += &chr.to_string();
        }
    }

    println!("Part 2: {}", program.output);
}
