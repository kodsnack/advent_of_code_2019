pub mod spif {
    use std::collections::{VecDeque, HashMap};
    use std::borrow::BorrowMut;
    use itertools::Itertools;
    use std::cmp::Ordering;

    pub fn parse_pixels(pixels: Vec<u32>, width: u8, height: u8) -> u32 {
        let size: usize = (width * height) as usize;
        //let mut layers: [VecDeque<u8>; SIZE] = [VecDeque::with_capacity(pixels.len() % SIZE); SIZE];
        //let image: [u8; pixels.len()] = [0; pixels.len()];
        let layers: HashMap<usize, Vec<u8>> = pixels.iter().enumerate()
            .map(|(index, pixel)| {
                let i: usize = index % size;
                (i, *pixel as u8)
            })
            .into_group_map();

        let sorted_layers: Vec<usize> = layers.iter()
            .inspect(|(i, l)| println!("{}: {:?}", i, l))
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
            //.inspect(|l| println!("{:?}", l))
            .map(|(index, _)| *index)
            .collect();

        let layer: usize = *sorted_layers.last().unwrap();
        let layer: &Vec<u8> = layers.get(&layer).unwrap();
        let (one, two) = layer.iter()
            .filter(|n| **n != 0)
            .partition::<Vec<u8>, _>(|n| **n == 1);
        println!("{:?}, {:?}", one, two);
        return (one.len() * two.len()) as u32
    }
}

#[cfg(test)]
mod test {
    use crate::lib::spif::parse_pixels;

    #[test]
    fn test_small_input() {      //  0  1  2  3  0  1  2  3  0  1  2  3
        let program: Vec<u32> = vec![1, 1, 2, 0, 2, 0, 2, 1, 1, 2, 0, 2];
        // 0 = 1, 2, 1 <-- (1, 1) * (2) ==> 2 * 1 = 2
        // 1 = 1, 0, 2
        // 2 = 2, 2, 0
        // 3 = 0, 1, 2
        let result: u32 = parse_pixels(program, 2, 2);
        assert_eq!(2, result)
    }
}