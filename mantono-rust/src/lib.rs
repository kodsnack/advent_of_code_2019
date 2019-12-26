pub mod spif {
    use std::collections::{HashMap};
    use itertools::Itertools;
    use std::cmp::Ordering;
    use std::fmt::{Display, Formatter, Write, Error};

    pub fn parse_pixels(pixels: Vec<u32>, width: u8, height: u8) -> u32 {
        let layers: HashMap<usize, Vec<u8>> = build_layers(pixels, width, height);

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

    pub fn find_layers(pixels: Vec<u32>, width: u8, height: u8) -> HashMap<usize, Vec<u8>> {
        let size: usize = (width * height) as usize;
        debug_assert_eq!(pixels.len() % size, 0);
        pixels.iter()
            .enumerate()
            .map(|(index, pixel)| {
                let i: usize = index % size;
                (i, *pixel as u8)
            })
            .into_group_map()
    }

    pub fn build_layers(pixels: Vec<u32>, width: u8, height: u8) -> HashMap<usize, Vec<u8>> {
        let size: usize = (width * height) as usize;
        debug_assert_eq!(pixels.len() % size, 0);
        pixels.iter()
            .enumerate()
            .map(|(index, pixel)| {
                let i: usize = index / size;
                (i, *pixel as u8)
            })
            .into_group_map()
    }

    pub fn reduce_image(pixels: Vec<u32>, width: u8, height: u8) -> Vec<Color> {
        let layers: HashMap<usize, Vec<u8>> = find_layers(pixels, width, height);
        debug_assert_eq!(layers.len(), (width * height) as usize);
        layers.iter()
            .sorted_by_key(|(&index, _)| index)
            .map(|(_, pixel)| {
                let p = pixel.iter()
                    .map(|n| Color::from_number(*n))
                    .fold(Color::Transparent, { |current, next|
                        current.reduce(next)
                    });
                p
            })
            .collect()
    }

    pub fn render_image(pixels: Vec<u32>, width: u8, height: u8) {
        let pixels: Vec<Color> = reduce_image(pixels, width, height);
        let width: usize = width as usize;
        let size = width * (height as usize);
        debug_assert_eq!(pixels.len(), size, "Wrong size, should be {} but was {}", size, pixels.len());

        pixels.iter()
            .enumerate()
            .map(|(i, c)| {
                if i % width == width - 1 {
                    format!("{}\n", c)
                } else {
                    format!("{}", c)
                }
            })
            .for_each(|s| print!("{}", s));
    }

    #[derive(Debug, PartialEq)]
    pub enum Color {
        Transparent,
        White,
        Black
    }

    impl Display for Color {
        fn fmt<'a>(&self, f: &mut Formatter<'a>) -> Result<(), Error> {
            match self {
                Color::Black => f.write_char('.'),
                Color::White => f.write_char('#'),
                Color::Transparent => f.write_char(' ')
            }
        }
    }

    impl Color {
        pub fn from_number(n: u8) -> Color {
            match n {
                0 => Color::Black,
                1 => Color::White,
                2 => Color::Transparent,
                _ => panic!("Invalid number")
            }
        }

        pub fn reduce<'a>(self, other: Color) -> Color {
            match self {
                Color::Black | Color::White => self,
                Color::Transparent => other
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::spif::{parse_pixels, reduce_image, find_layers, Color};
    use std::collections::HashMap;
    use std::collections::hash_map::RandomState;

    #[test]
    fn test_small_input() {      //  0  1  2  3  0  1  2  3  0  1  2  3
        let program: Vec<u32> = vec![1, 1, 2, 0, 2, 2, 2, 1, 1, 2, 0, 2];
        // Layer     0  1  2
        // ------------------
        // Pixel 0 = 1, 2, 1
        // Pixel 1 = 1, 2, 2
        // Pixel 2 = 2, 2, 0
        // Pixel 3 = 0, 1, 2
        // Total ------------
        //           1  0  1
        // 1 * 3
        let result: u32 = parse_pixels(program, 2, 2);
        assert_eq!(3, result)
    }

    #[test]
    fn test_reduce_image() {
        //                         B, T, T, T, W, W, T, T, T, T, W, T, B, B, B, B
        let input: Vec<u32> = vec!(0, 2, 2, 2, 1, 1, 2, 2, 2, 2, 1, 2, 0, 0, 0, 0);
        // Layer 1                 B, T, T, T => B
        // Layer 2                 W, W, T, T => W
        // Layer 3                 T, T, W, T => W
        // Layer 4                 B, B, B, B => B
        let expected: Vec<Color> = vec!(Color::Black, Color::White, Color::White, Color::Black);
        assert_eq!(expected, reduce_image(input, 2, 2))
    }

    #[test]
    fn test_find_layers() {
        let input: Vec<u32> = vec!(0, 2, 2, 2, 1, 1, 2, 2, 2, 2, 1, 2, 0, 0, 0, 0);
        let expected: HashMap<usize, Vec<u8>, RandomState> = maplit::hashmap!{
            0 => vec!(0, 2, 2, 2),
            1 => vec!(1, 1, 2, 2),
            2 => vec!(2, 2, 1, 2),
            3 => vec!(0, 0, 0, 0),
        };
        let result = find_layers(input, 2, 2);
        assert_eq!(expected, result);
    }
}