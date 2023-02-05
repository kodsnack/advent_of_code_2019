import '../util/util.dart';

//const String inputFile = 'day8/example.txt';
const String inputFile = 'day8/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  print('Part 2:');
  calcResultP2(input);
}

int calcResultP1(String input) {
  int imgWidth = 25;
  int imgHeight = 6;
  List<Layer> layers = [];
  final pixels = input.split('').map((e) => int.parse(e)).toList();

  int imgSize = imgHeight * imgWidth;
  for (int idx = 0; idx < pixels.length; idx += imgSize) {
    layers.add(Layer(pixels.sublist(idx, idx + imgSize), imgWidth, imgHeight));
  }

  int fewest0Digits = veryLargeNumber;
  int digit1Count = 0;
  int digit2Count = 0;
  for (final layer in layers) {
    int digit0Count = layer.count(0);
    if (digit0Count < fewest0Digits) {
      fewest0Digits = digit0Count;
      digit1Count = layer.count(1);
      digit2Count = layer.count(2);
    }
  }

  return digit1Count * digit2Count;
}

calcResultP2(String input) {
  int imgWidth = 25;
  int imgHeight = 6;
  List<Layer> layers = [];
  final pixels = input.split('').map((e) => int.parse(e)).toList();

  int imgSize = imgHeight * imgWidth;
  for (int idx = 0; idx < pixels.length; idx += imgSize) {
    layers.add(Layer(pixels.sublist(idx, idx + imgSize), imgWidth, imgHeight));
  }

  List<List<int>> image = [];
  for (int row = 0; row < imgHeight; row++) {
    List<int> imgRow = [];
    for (int col = 0; col < imgWidth; col++) {
      int pixel = 2;
      int layerNo = 0;
      while (pixel == 2 && layerNo < layers.length) {
        pixel = layers[layerNo].pixels[row][col];
        layerNo++;
      }
      imgRow.add(pixel);
    }
    image.add(imgRow);
  }

  for (int row = 0; row < imgHeight; row++) {
    String line = image[row].map((e) => e == 0 ? ' ' : '#').join();
    print(line);
  }
}

class Layer {
  List<List<int>> pixels = [];
  int width = 0;
  int height = 0;

  Layer(List<int> pixelStream, this.width, this.height) {
    for (int idx = 0; idx < pixelStream.length; idx += width) {
      List<int> pixelRow = [...pixelStream.sublist(idx, idx + width)];
      pixels.add(pixelRow);
    }
  }

  int count(int pixelToCount) {
    int count = 0;
    for (final row in pixels) {
      for (final col in row) {
        if (col == pixelToCount) count++;
      }
    }
    return count;
  }
}
