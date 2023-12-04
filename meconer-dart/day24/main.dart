import '../util/linepos.dart';
import '../util/lprange.dart';
import '../util/util.dart';

// const String inputFile = 'day24/example.txt';
const String inputFile = 'day24/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  // print('Part 2:');
  // final resultP2 = calcResultP2(input);
  // print(resultP2);
}

int calcResultP1(String input) {
  Grid grid = Grid.from(input);
  grid.show();
  Set<int> biodiversities = {};
  biodiversities.add(grid.calcBioDiversity());
  while (true) {
    final nextGrid = grid.evolve();
    nextGrid.show();
    int bioDiversity = nextGrid.calcBioDiversity();
    if (biodiversities.contains(bioDiversity)) return bioDiversity;
    biodiversities.add(bioDiversity);
    grid = nextGrid;
  }
}

class Grid {
  late List<List<String>> spots;
  late int width;
  late int height;

  Grid.from(String input) {
    final lines = input.split('\n');
    height = lines.length;
    width = lines[0].length;
    spots = [];
    for (final line in lines) {
      List<String> spotLine = [];
      for (final char in line.split('')) {
        spotLine.add(char);
      }
      spots.add(spotLine);
    }
  }

  Grid(int rows, int cols) {
    this.width = cols;
    this.height = rows;
    spots = [];
    for (int row = 0; row < rows; row++) {
      List<String> spotLine = [];
      for (int col = 0; col < rows; col++) {
        spotLine.add('.');
      }
      spots.add(spotLine);
    }
  }

  Grid evolve() {
    Grid newGrid = Grid(5, 5);
    LPRange range = LPRange();
    range.extend(LinePos(0, 0));
    range.extend(LinePos(4, 4));
    for (int row = 0; row < height; row++) {
      for (int col = 0; col < height; col++) {
        final lpos = LinePos(col, row);
        final neighbours = lpos.getNeighbours();
        int bugCount = 0;
        for (final neighbour in neighbours) {
          if (range.contains(neighbour) &&
              spots[neighbour.row][neighbour.col] == '#') bugCount++;
        }
        // A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
        if (spots[row][col] == '#') {
          if (bugCount == 1) {
            newGrid.spots[row][col] = '#';
          } else {
            newGrid.spots[row][col] = '.';
          }
        } else {
          // An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
          if (bugCount == 2 || bugCount == 1) {
            newGrid.spots[row][col] = '#';
          } else {
            newGrid.spots[row][col] = '.';
          }
        }
      }
    }
    return newGrid;
  }

  void show() {
    print('');
    for (int row = 0; row < height; row++) {
      String line = '';
      for (int col = 0; col < width; col++) {
        line += spots[row][col];
      }
      print(line);
    }
  }

  int calcBioDiversity() {
    int posValue = 1;
    int totalValue = 0;
    for (int row = 0; row < height; row++) {
      for (int col = 0; col < width; col++) {
        if (spots[row][col] == '#') totalValue += posValue;
        posValue *= 2;
      }
    }
    return totalValue;
  }
}
