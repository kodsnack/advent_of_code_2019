pub fn run() -> String {
  let data = loader::load_as("./day/08/input.csv", |l| l).collect::<Vec<String>>()[0].clone();
  let image = Image::from_string(25, 6, data);
  write_image(&image);
  "whee!".to_owned()
}

fn write_image(image: &Image) {
  let composit = image.merge();
  for ih in 0..image.height {
    for iw in 0..image.width {
      if composit[ih * image.width + iw] == 1 {
        print!("█");
      } else {
        print!(" ");
      }
    }
    println!();
  }
}

/**
* * *****   *     **
 **     * *     *  **
 ***    **  * **  **
 *  **  * * **
 *  *    *    *  * ****
 */

#[derive(Clone, Debug)]
pub struct ImageLayer {
  pixels: Vec<u8>,
}
impl ImageLayer {
  pub fn count(&self, value: u8) -> usize {
    self
      .pixels
      .iter()
      .cloned()
      .filter(|pixel| *pixel == value)
      .count()
  }
  pub fn get(&self, pos: usize) -> u8 {
    self.pixels[pos]
  }
  pub fn pixels(&self) -> Vec<u8> {
    self.pixels.clone()
  }
  pub fn load(stride: usize, data: &[u8]) -> ImageLayer {
    assert!(data.len() >= stride);
    ImageLayer {
      pixels: data.iter().take(stride).cloned().collect(),
    }
  }
}

#[derive(Clone, Debug)]
pub struct Image {
  pub width: usize,
  pub height: usize,
  stride: usize,
  layers: Vec<ImageLayer>,
}

fn digit(c: char) -> u8 {
  match c {
    '0' => 0,
    '1' => 1,
    '2' => 2,
    '3' => 3,
    '4' => 4,
    '5' => 5,
    '6' => 6,
    '7' => 7,
    '8' => 8,
    '9' => 9,
    _ => panic!("bad digit"),
  }
}

impl Image {
  pub fn from_string(width: usize, height: usize, data: String) -> Image {
    let stride = width * height;
    assert_eq!(data.len() % stride, 0);
    let data = data.chars().map(digit).collect::<Vec<u8>>();
    let mut layers: Vec<ImageLayer> = Vec::new();
    let mut idx = 0;
    loop {
      let layer: Vec<u8> = data
        .iter()
        .skip(idx * stride)
        .take(stride)
        .cloned()
        .collect();
      if layer.len() != stride {
        if layer.is_empty() {
          break;
        }
        panic!("invalid data");
      }
      idx += 1;
      layers.push(ImageLayer::load(stride, &layer));
    }
    Image {
      width,
      height,
      stride,
      layers,
    }
  }
  pub fn layer(&self, idx: usize) -> Option<ImageLayer> {
    self.layers.get(idx - 1).cloned()
  }
  pub fn layers(&self) -> usize {
    self.layers.len()
  }
  pub fn layers_with_fewest(&self, value: u8) -> Vec<ImageLayer> {
    let min_count = self
      .layers
      .iter()
      .cloned()
      .map(|l| l.count(value))
      .min()
      .expect("We should have one layer, right? Guys?");
    self
      .layers
      .iter()
      .cloned()
      .filter(|l| l.count(value) == min_count)
      .collect()
  }
  pub fn merge(&self) -> Vec<u8> {
    let mut hm: std::collections::HashMap<usize, u8> = std::collections::HashMap::new();
    for layer in 0..self.layers() {
      self.layers[layer]
        .pixels
        .iter()
        .enumerate()
        .for_each(|(idx, value)| {
          if !hm.contains_key(&idx) && *value != 2 {
            hm.insert(idx, *value);
          }
        });
    }
    let mut composit: Vec<(usize, u8)> = hm.into_iter().collect();
    composit.sort();
    composit.iter().cloned().map(|(_pos, pix)| pix).collect()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  #[test]
  fn mini_image_1() {
    let image = Image::from_string(3, 2, "123456789012".to_owned());
    assert_eq!(image.layers(), 2);
  }
  #[test]
  fn mini_image_2() {
    let image = Image::from_string(3, 2, "123456789012".to_owned());
    assert_eq!(image.layer(1).unwrap().pixels(), vec![1, 2, 3, 4, 5, 6]);
  }
  #[test]
  fn mini_image_3() {
    let image = Image::from_string(3, 2, "123456789012".to_owned());
    assert_eq!(image.layer(2).unwrap().pixels(), vec![7, 8, 9, 0, 1, 2]);
  }
  #[test]
  fn mini_image_4() {
    let image = Image::from_string(3, 2, "123456789012".to_owned());
    assert_eq!(
      image.layers_with_fewest(0).pop().unwrap().pixels(),
      vec![1, 2, 3, 4, 5, 6]
    );
  }
  #[test]
  fn mini_image_5() {
    let image = Image::from_string(2, 2, "0222112222120000".to_owned());
    assert_eq!(image.layer(1).unwrap().pixels(), vec![0, 2, 2, 2]);
  }
  #[test]
  fn mini_image_6() {
    let image = Image::from_string(2, 2, "0222112222120000".to_owned());
    assert_eq!(image.layer(2).unwrap().pixels(), vec![1, 1, 2, 2]);
  }
  #[test]
  fn mini_image_7() {
    let image = Image::from_string(2, 2, "0222112222120000".to_owned());
    assert_eq!(image.layer(3).unwrap().pixels(), vec![2, 2, 1, 2]);
  }
  #[test]
  fn mini_image_8() {
    let image = Image::from_string(2, 2, "0222112222120000".to_owned());
    assert_eq!(image.layer(4).unwrap().pixels(), vec![0, 0, 0, 0]);
  }
  #[test]
  fn mini_image_9() {
    let image = Image::from_string(2, 2, "0222112222120000".to_owned());
    assert_eq!(image.merge(), vec![0, 1, 1, 0]);
  }
  /*
  222222222212222022202222212102221220122222
  */
  #[test]
  fn midi_image_1() {
    let image = Image::from_string(
      3,
      2,
      "222222222212222022202222212102221220122222".to_owned(),
    );
    println!();
    write_image(&image);
    assert_eq!(image.merge(), vec![1, 0, 1, 0, 1, 0]);
  }
  #[test]
  fn image_1() {
    let data = loader::load_as("../day/08/input.csv", |l| l).collect::<Vec<String>>()[0].clone();
    let image = Image::from_string(25, 6, data);
    assert_eq!(image.layers(), 100);
  }
  #[test]
  fn image_2() {
    let data = loader::load_as("../day/08/input.csv", |l| l).collect::<Vec<String>>()[0].clone();
    let image = Image::from_string(25, 6, data);
    let layers = image.layers_with_fewest(0);
    assert_eq!(layers.len(), 1);
  }
  #[test]
  fn image_3() {
    let data = loader::load_as("../day/08/input.csv", |l| l).collect::<Vec<String>>()[0].clone();
    let image = Image::from_string(25, 6, data);
    let layer = image.layers_with_fewest(0).pop().unwrap();
    assert_eq!(layer.count(1), 17);
  }
  #[test]
  fn image_4() {
    let data = loader::load_as("../day/08/input.csv", |l| l).collect::<Vec<String>>()[0].clone();
    let image = Image::from_string(25, 6, data);
    let layer = image.layers_with_fewest(0).pop().unwrap();
    assert_eq!(layer.count(2), 127);
  }
  #[test]
  fn image_5() {
    let data = loader::load_as("../day/08/input.csv", |l| l).collect::<Vec<String>>()[0].clone();
    let image = Image::from_string(25, 6, data);
    let layer = image.layers_with_fewest(0).pop().unwrap();
    assert_eq!(layer.count(1) * layer.count(2), 2159);
  }
  #[test]
  fn image_6() {
    let data = loader::load_as("../day/08/input.csv", |l| l).collect::<Vec<String>>()[0].clone();
    let image = Image::from_string(25, 6, data);
    println!();
    write_image(&image);
    assert_eq!(image.merge(), vec![]);
  }
}
