pub mod spif {
    use std::collections::{VecDeque, HashMap};
    use std::borrow::BorrowMut;
    use itertools::Itertools;
    use std::cmp::Ordering;

    const WIDTH: usize = 25;
    const HEIGHT: usize = 6;
    const SIZE: usize = WIDTH * HEIGHT;

    pub fn parse_pixels(pixels: Vec<u32>) -> u32 {
        //let mut layers: [VecDeque<u8>; SIZE] = [VecDeque::with_capacity(pixels.len() % SIZE); SIZE];
        //let image: [u8; pixels.len()] = [0; pixels.len()];
        let layers: HashMap<usize, Vec<u8>> = pixels.iter().enumerate()
            .map(|(index, pixel)| {
                let i: usize = index % SIZE;
                (i, *pixel as u8)
            })
            .into_group_map();

        let sorted_layers: Vec<usize> = layers.iter()
            .map(|(i, p)| {
                let sum: usize = p.iter().filter(|n| **n != 0u8).count();
                (i, sum)
            })
            .sorted_by(|(_, sum0), (_, sum1)|{
                if sum0 < sum1 {
                    Ordering::Less
                } else if sum0 > sum1 {
                    Ordering::Greater
                } else {
                    Ordering::Equal
                }
            })
            .map(|(index, _)| *index)
            .collect();

        let layer: usize = *sorted_layers.first().unwrap();
        let layer: &Vec<u8> = layers.get(&layer).unwrap();
        let (one, two) = layer.iter()
            .filter(|n| **n != 0)
            .partition::<Vec<u8>, _>(|n| **n == 1);
        println!("{:?}, {:?}", one, two);
        return (one.len() * two.len()) as u32
    }
}