extern crate ggez;

use ggez::event::{KeyCode, KeyMods};
use ggez::{event, graphics, Context, GameResult};
use std::cmp;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::time::{Duration, Instant};

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

    let mut screen = HashMap::<Point, i64>::new();

    let mut width = 0;
    let mut height = 0;

    let mut program = IntCode::new(v_orig.clone());
    loop {
        program.run(&vec![]);
        if program.halted {
            break;
        }
        let x = program.output;
        program.run(&vec![]);
        let y = program.output;
        program.run(&vec![]);
        let object = program.output;

        width = cmp::max(width, x);
        height = cmp::max(height, y);

        screen.insert(Point { x, y }, object);
    }

    width += 1;
    height += 1;

    let mut num_2 = 0;
    for (_point, object) in screen.iter() {
        if *object == 2 {
            num_2 += 1;
        }
    }

    println!("Part 1: {}", num_2);

    //    part_2_ggez(width, height, v_orig.clone()).unwrap();
    let score = part_2_just_solve_it(width, height, v_orig.clone());

    println!("Part 2: {}", score);
}

fn part_2_just_solve_it(width: i64, height: i64, code: Vec<i64>) -> i64 {
    let state = &mut GameState::new(width, height, code);
    state.program.program[0] = 2;

    loop {
        state.program.run(&vec![state.joystick]);
        if state.program.halted {
            break;
        }
        let x = state.program.output;
        state.program.run(&vec![state.joystick]);
        let y = state.program.output;
        state.program.run(&vec![state.joystick]);
        let object = state.program.output;

        if x == -1 && y == 0 {
            state.score = object;
            continue;
        } else {
            if object == 3 {
                state.paddle_x = x;
            } else if object == 4 {
                state.joystick = if state.paddle_x < x {
                    1
                } else if state.paddle_x > x {
                    -1
                } else {
                    0
                };
            }
        }
    }

    state.score
}

const UPDATES_PER_SECOND: f32 = 60.0;
const MILLIS_PER_UPDATE: u64 = (1.0 / UPDATES_PER_SECOND * 1000.0) as u64;
const POINT_SIZE: i32 = 16;

impl From<Point> for graphics::Rect {
    fn from(point: Point) -> Self {
        graphics::Rect::new_i32(
            point.x as i32 * POINT_SIZE,
            point.y as i32 * POINT_SIZE,
            POINT_SIZE,
            POINT_SIZE,
        )
    }
}

struct GameState {
    program: IntCode,
    width: i64,
    height: i64,
    screen: Vec<i64>,
    score: i64,
    last_update: Instant,
    init_update: i64,
    paddle_x: i64,
    joystick: i64,
}

impl GameState {
    fn new(width: i64, height: i64, code: Vec<i64>) -> GameState {
        GameState {
            program: IntCode::new(code),
            width,
            height,
            screen: Vec::<i64>::new(),
            score: 0,
            last_update: Instant::now(),
            init_update: 720,
            paddle_x: 0,
            joystick: 0,
        }
    }
}

impl event::EventHandler for GameState {
    fn update(&mut self, ctx: &mut Context) -> GameResult {
        if Instant::now() - self.last_update < Duration::from_millis(MILLIS_PER_UPDATE) {
            return Ok(());
        }

        loop {
            self.program.run(&vec![self.joystick]);
            if self.program.halted {
                event::quit(ctx);
                return Ok(());
            }
            let x = self.program.output;
            self.program.run(&vec![self.joystick]);
            let y = self.program.output;
            self.program.run(&vec![self.joystick]);
            let object = self.program.output;

            if x == -1 && y == 0 {
                self.score = object;
                println!("score: {}", self.score);
                continue;
            } else {
                self.screen[(y * self.width + x) as usize] = object;

                if object == 3 {
                    self.paddle_x = x;
                } else if object == 4 {
                    self.joystick = if self.paddle_x < x {
                        1
                    } else if self.paddle_x > x {
                        -1
                    } else {
                        0
                    };
                }

                if self.init_update == 0 {
                    break;
                }
                self.init_update -= 1;
            }
        }

        self.last_update = Instant::now();

        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult {
        graphics::clear(ctx, [0.0, 0.0, 0.0, 1.0].into());

        for y in 0..self.height {
            for x in 0..self.width {
                let object = self.screen[(y * self.width + x) as usize];
                let color = match object {
                    0 => [0.0, 0.0, 0.0, 1.0].into(),
                    1 => [0.6, 0.6, 0.6, 1.0].into(),
                    2 => [0.8, 0.8, 0.0, 1.0].into(),
                    3 => [0.9, 0.9, 0.9, 1.0].into(),
                    4 => [0.7, 0.0, 0.0, 1.0].into(),
                    _ => [0.0, 0.0, 0.0, 1.0].into(),
                };

                let rectangle = graphics::Mesh::new_rectangle(
                    ctx,
                    graphics::DrawMode::fill(),
                    Point { x, y }.into(),
                    color,
                )?;
                graphics::draw(ctx, &rectangle, (ggez::mint::Point2 { x: 0.0, y: 0.0 },))?;
            }
        }

        graphics::present(ctx)?;
        ggez::timer::yield_now();
        Ok(())
    }

    /// key_down_event gets fired when a key gets pressed.
    fn key_down_event(
        &mut self,
        _ctx: &mut Context,
        keycode: KeyCode,
        _keymod: KeyMods,
        _repeat: bool,
    ) {
        self.joystick = match keycode {
            KeyCode::Left => -1,
            KeyCode::Right => 1,
            _ => 0,
        };
    }
}

fn part_2_ggez(width: i64, height: i64, code: Vec<i64>) -> GameResult {
    let (ctx, events_loop) = &mut ggez::ContextBuilder::new("day12", "Tomas Skare")
        .window_setup(ggez::conf::WindowSetup::default().title("Day12"))
        .window_mode(ggez::conf::WindowMode::default().dimensions(
            width as f32 * POINT_SIZE as f32,
            height as f32 * POINT_SIZE as f32,
        ))
        .build()?;

    let state = &mut GameState::new(width, height, code);
    state.program.program[0] = 2;
    state.screen.resize_with((width * height) as usize, || 0);
    event::run(ctx, events_loop, state)
}
