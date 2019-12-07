use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

#[derive(Debug)]
struct Node {
    name: String,
    parent: Option<String>,
}

fn count_parents(map: &HashMap<String, Node>, name: &Option<String>) -> u32 {
    if let Some(n) = name {
        let node = map.get(n).unwrap();
        count_parents(map, &node.parent) + 1
    } else {
        0
    }
}

fn count_map_parents(
    map: &HashMap<String, Node>,
    nummap: &mut HashMap<String, u32>,
    name: &Option<String>,
) -> u32 {
    if let Some(n) = name {
        let node = map.get(n).unwrap();
        let num = count_map_parents(map, nummap, &node.parent) + 1;
        nummap.insert(n.clone(), num);
        num
    } else {
        0
    }
}

fn find_common_parent(
    map: &HashMap<String, Node>,
    nummap: &HashMap<String, u32>,
    name: &Option<String>,
) -> Option<String> {
    if let Some(n) = name {
        if nummap.contains_key(n) {
            Some(n.to_string())
        } else {
            let node = map.get(n).unwrap();
            find_common_parent(map, nummap, &node.parent)
        }
    } else {
        None
    }
}

fn main() {
    let filename = "input.txt";

    let fd = File::open(filename).expect(&format!("Failure opening {}", filename));
    let buf = BufReader::new(fd);

    let mut map = HashMap::<String, Node>::new();

    buf.lines().for_each(|line| {
        let lineu = line.unwrap();
        let v: Vec<&str> = lineu.split(')').collect();
        let (inner, outer) = (String::from(v[0]), String::from(v[1]));

        let inner_node = map.get(&inner);
        if inner_node.is_none() {
            map.insert(
                inner.clone(),
                Node {
                    name: inner.clone(),
                    parent: None,
                },
            );
        }

        let outer_node = map.get_mut(&outer);
        if let Some(o) = outer_node {
            o.parent = Some(inner);
        } else {
            map.insert(
                outer.clone(),
                Node {
                    name: outer.clone(),
                    parent: Some(inner),
                },
            );
        }
    });

    let mut num_parents = 0;
    for (_key, val) in map.iter() {
        num_parents += count_parents(&map, &val.parent);
    }

    println!("Part 1: {}", num_parents);

    let you_node = map.get(&String::from("YOU")).unwrap();
    let san_node = map.get(&String::from("SAN")).unwrap();

    let mut you_nummap = HashMap::<String, u32>::new();
    let you_start_num = count_map_parents(&map, &mut you_nummap, &you_node.parent);

    let mut san_nummap = HashMap::<String, u32>::new();
    let san_start_num = count_map_parents(&map, &mut san_nummap, &san_node.parent);

    let common = find_common_parent(&map, &you_nummap, &san_node.parent).unwrap();

    let you_common_num = you_nummap.get(&common).unwrap();
    let you_num = you_start_num - you_common_num;

    let san_common_num = san_nummap.get(&common).unwrap();
    let san_num = san_start_num - san_common_num;

    println!("Part 2: {}", you_num + san_num);
}
