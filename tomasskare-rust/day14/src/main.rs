extern crate regex;

use regex::Regex;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

#[derive(Debug)]
struct Reaction {
    input: HashMap<String, u64>,
    output_type: String,
    output_num: u64,
}

fn main() {
    let filename = "input.txt";

    let fd = File::open(filename).expect(&format!("Failure opening {}", filename));
    let buf = BufReader::new(fd);

    let mut reactions = HashMap::<String, Reaction>::new();

    let react_re = Regex::new(r"(.+) => (.+)").unwrap();
    buf.lines().for_each(|line| {
        let lineu = line.unwrap();
        for cap in react_re.captures_iter(&lineu) {
            let input = &cap[1];
            let output = &cap[2];

            let ochemnum: Vec<&str> = output.split(' ').collect();
            let mut reaction = Reaction {
                input: HashMap::<String, u64>::new(),
                output_type: ochemnum[1].to_string(),
                output_num: ochemnum[0].parse::<u64>().unwrap(),
            };

            for inp in input.split(',') {
                let chem = inp.trim();
                let ochemnum: Vec<&str> = chem.split(' ').collect();
                let chem = ochemnum[1].to_string();
                let num = ochemnum[0].parse::<u64>().unwrap();
                reaction.input.insert(chem, num);
            }
            reactions.insert(reaction.output_type.clone(), reaction);
        }
    });

    let mut stash = HashMap::<String, u64>::new();
    let num_ore = resolve(&mut stash, &reactions, &"FUEL".to_string(), 1, 1);
    println!("Part 1: {}", num_ore);

    let mut stash = HashMap::<String, u64>::new();
    let mut total_ore = 1000000000000u64;
    let mut num_fuel = 0;
    let mut factor = 32768;
    // This half_threshold works for my input, may not work for others.
    // I'm not very satisfied with this, but can't think of any better
    // solution right now.
    let half_threshold = 64;
    loop {
        let mut stash_copy = stash.clone();
        let num_ore = resolve(
            &mut stash_copy,
            &reactions,
            &"FUEL".to_string(),
            factor,
            factor,
        );
        if factor == 1 && total_ore < num_ore {
            break;
        } else if factor > 1 && total_ore < num_ore * half_threshold {
            factor /= 2;
            continue;
        }
        total_ore -= num_ore;
        num_fuel += factor;
        stash = stash_copy;
    }

    println!("Part 2: {}", num_fuel);
}

fn resolve(
    mut stash: &mut HashMap<String, u64>,
    reactions: &HashMap<String, Reaction>,
    needchem: &String,
    neednum: u64,
    factor: u64,
) -> u64 {
    let mut total_ore = 0;
    let mut output_curr_num = *stash.get(needchem).unwrap_or(&0);
    if output_curr_num >= neednum {
        return 0;
    }

    let need_reaction = reactions.get(needchem).unwrap();

    while output_curr_num < neednum {
        for (chem, num) in need_reaction.input.iter() {
            let num = factor * *num;
            let mut currnum = *stash.get(chem).unwrap_or(&0);
            if *chem == "ORE".to_string() {
                if currnum < num {
                    total_ore += num;
                    currnum += num;
                }
            } else {
                total_ore += resolve(&mut stash, &reactions, chem, num, factor);
                currnum = *stash.get(chem).unwrap_or(&0);
            }

            currnum -= num;
            stash.insert(chem.clone(), currnum);
        }

        output_curr_num += factor * need_reaction.output_num;
    }

    if *needchem != "FUEL".to_string() {
        stash.insert(needchem.clone(), output_curr_num);
    }
    total_ore
}
