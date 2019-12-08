use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

#[derive(Debug)]
struct Layer {
    values: Vec<i32>,
    num_0: usize,
    num_1: usize,
    num_2: usize,
}

fn main() {
    let filename = "input.txt";

    let fd = File::open(filename).expect(&format!("Failure opening {}", filename));
    let buf = BufReader::new(fd);
    let mut v = Vec::<i32>::new();
    buf.lines().for_each(|line| {
        line.unwrap().chars().for_each(|chr| {
            let num = chr.to_string().parse::<i32>().unwrap();
            v.push(num);
        });
    });

    let width = 25;
    let height = 6;

    let size: usize = width * height;

    let mut layers = Vec::<Layer>::new();

    let mut i = 0;
    while i < v.len() {
        let mut layer = Layer {
            values: Vec::<i32>::new(),
            num_0: 0,
            num_1: 0,
            num_2: 0,
        };
        for _ in 0..size {
            let num = v[i];
            layer.values.push(num);
            if num == 0 {
                layer.num_0 += 1;
            } else if num == 1 {
                layer.num_1 += 1;
            } else if num == 2 {
                layer.num_2 += 1;
            }
            i += 1;
        }
        layers.push(layer);
    }

    let mut min_0 = size;
    let mut min_0_layer = layers.len();
    for l in 0..layers.len() {
        if layers[l].num_0 < min_0 {
            min_0 = layers[l].num_0;
            min_0_layer = l;
        }
    }

    let sum = layers[min_0_layer].num_1 * layers[min_0_layer].num_2;

    println!("Part 1: {}", sum);

    let mut image: Vec<i32> = vec![2; size];
    for i in 0..size {
        for layer in &layers {
            if image[i] == 2 {
                image[i] = layer.values[i];
            }
        }
    }

    println!("Part 2:");
    for y in 0..height {
        for x in 0..width {
            print!("{}", if image[y * width + x] == 1 { "#" } else { " " });
        }
        print!("\n");
    }
}
