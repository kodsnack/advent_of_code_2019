import '../util/linepos.dart';
import '../util/util.dart';

// const String inputFile = 'day24/example.txt';
const String inputFile = 'day24/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInputAsString(inputFile);

  print('Part 2:');
  final resultP2 = calcResultP2(input);
  print(resultP2);
}

int calcResultP2(String input) {
  Grid grid = Grid.from(input);
  Map<int, Grid> grids = {0: grid};
  for (final grid in grids.values) {
    print('Level : ${grid.level}');
    grid.show();
  }
  for (int t = 0; t < 200; t++) {
    grids[0]!.evolve(grids);
    final gridList = grids.values.toList();
    gridList.sort((a, b) => a.level.compareTo(b.level));
    for (final grid in gridList) {
      grid.show();
    }
  }
  int sum = 0;
  for (final grid in grids.values) {
    sum += grid.countBugs();
  }
  return sum;
}

final innerRing = [
  LinePos(1, 1),
  LinePos(2, 1),
  LinePos(3, 1),
  LinePos(1, 2),
  LinePos(3, 2),
  LinePos(1, 3),
  LinePos(2, 3),
  LinePos(3, 3),
];

final outerRing = [
  LinePos(0, 0),
  LinePos(1, 0),
  LinePos(2, 0),
  LinePos(3, 0),
  LinePos(4, 0),
  LinePos(0, 1),
  LinePos(4, 1),
  LinePos(0, 2),
  LinePos(4, 2),
  LinePos(0, 3),
  LinePos(4, 3),
  LinePos(0, 4),
  LinePos(1, 4),
  LinePos(2, 4),
  LinePos(3, 4),
  LinePos(4, 4),
];

class Grid {
  late List<List<String>> spots;
  late int width;
  late int height;
  Grid? midGrid;
  int level = 0;

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

  void evolve(Map<int, Grid> grids) {
    Grid newGrid = Grid(5, 5);
    newGrid.level = level;

    // First evolve this level
    // Start with outer ring
    for (final pos in outerRing) {
      int neighbouringBugCount = 0;
      if (pos.col == 0) {
        // Leftmost column. The left neighbour is on the level above (level -1)
        neighbouringBugCount += grids[level - 1]?.spots[2][1] == '#' ? 1 : 0;
        // right neighbour is on same level
        neighbouringBugCount += spots[pos.row][pos.col + 1] == '#' ? 1 : 0;
      } else if (pos.col == 4) {
        // Rightmost column. The right neighbour is on the level above (level -1)
        neighbouringBugCount += grids[level - 1]?.spots[2][3] == '#' ? 1 : 0;
        // left neighbour is on same level
        neighbouringBugCount += spots[pos.row][pos.col - 1] == '#' ? 1 : 0;
      } else {
        // Both left and right neighbours are on the same level
        neighbouringBugCount += spots[pos.row][pos.col + 1] == '#' ? 1 : 0;
        neighbouringBugCount += spots[pos.row][pos.col - 1] == '#' ? 1 : 0;
      }
      if (pos.row == 0) {
        // Top row. The neighbour above is on the level above (level -1)
        neighbouringBugCount += grids[level - 1]?.spots[1][2] == '#' ? 1 : 0;
        // Neighbour below is on same level
        neighbouringBugCount += spots[pos.row + 1][pos.col] == '#' ? 1 : 0;
      } else if (pos.row == 4) {
        // Bottom row. The neighbour below is on the level above (level -1)
        neighbouringBugCount += grids[level - 1]?.spots[3][2] == '#' ? 1 : 0;
        // Neighbour above is on same level
        neighbouringBugCount += spots[pos.row - 1][pos.col] == '#' ? 1 : 0;
      } else {
        // Both neighbours above and below are on the same level
        neighbouringBugCount += spots[pos.row + 1][pos.col] == '#' ? 1 : 0;
        neighbouringBugCount += spots[pos.row - 1][pos.col] == '#' ? 1 : 0;
      }
      // A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
      if (spots[pos.row][pos.col] == '#') {
        if (neighbouringBugCount == 1) {
          newGrid.spots[pos.row][pos.col] = '#';
        } else {
          newGrid.spots[pos.row][pos.col] = '.';
        }
      } else {
        // An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
        if (neighbouringBugCount == 2 || neighbouringBugCount == 1) {
          newGrid.spots[pos.row][pos.col] = '#';
        } else {
          newGrid.spots[pos.row][pos.col] = '.';
        }
      }
    }

    // Then the inner ring
    for (final pos in innerRing) {
      int neighbouringBugCount = 0;
      if (pos.col == 1) {
        // Left side.
        if (pos.row == 2) {
          // We get the right neighbour count from the entire leftmost column
          // on the inner level (level +1)
          // The code below should return the bugcount if the level exist or 0
          // otherwise
          neighbouringBugCount +=
              grids[level + 1]?.getLeftmostColumnBugCount() ?? 0;
        } else {
          // Not the middle position. The right neighbour is on the same level
          neighbouringBugCount += spots[pos.row][pos.col + 1] == '#' ? 1 : 0;
        }
        // left neighbour is on same level
        neighbouringBugCount += spots[pos.row][pos.col - 1] == '#' ? 1 : 0;
        // Neighbours above and below are on the same level
        neighbouringBugCount += spots[pos.row - 1][pos.col] == '#' ? 1 : 0;
        neighbouringBugCount += spots[pos.row + 1][pos.col] == '#' ? 1 : 0;
      } else if (pos.col == 3) {
        // Right side.
        if (pos.row == 2) {
          // We get the left neighbour count from the entire rightmost column
          // on the inner level (level +1)
          // The code below should return the bugcount if the level exist or 0
          // otherwise
          neighbouringBugCount +=
              grids[level + 1]?.getRightmostColumnBugCount() ?? 0;
        } else {
          // Not the middle position. The left neighbour is on the same level
          neighbouringBugCount += spots[pos.row][pos.col - 1] == '#' ? 1 : 0;
        }
        // right neighbour is on same level
        neighbouringBugCount += spots[pos.row][pos.col + 1] == '#' ? 1 : 0;
        // Neighbours above and below are on the same level
        neighbouringBugCount += spots[pos.row - 1][pos.col] == '#' ? 1 : 0;
        neighbouringBugCount += spots[pos.row + 1][pos.col] == '#' ? 1 : 0;
      } else {
        // pos.col == 2
        if (pos.row == 1) {
          // Top.
          // We get the neighbour below count from the entire top row
          // on the inner level (level +1)
          // The code below should return the bugcount if the level exist or 0
          // otherwise
          neighbouringBugCount += grids[level + 1]?.getTopRowBugCount() ?? 0;
          // The neighbour above is on same level
          neighbouringBugCount += spots[pos.row - 1][pos.col] == '#' ? 1 : 0;
        } else {
          // pos.row == 3
          // Bottom
          // We get the neighbour above count from the entire bottom row
          // on the inner level (level +1)
          // The code below should return the bugcount if the level exist or 0
          // otherwise
          neighbouringBugCount += grids[level + 1]?.getBottomRowBugCount() ?? 0;
          // The neighbour below is on same level
          neighbouringBugCount += spots[pos.row + 1][pos.col] == '#' ? 1 : 0;
        }
        // left and right neighbours is on same level
        neighbouringBugCount += spots[pos.row][pos.col - 1] == '#' ? 1 : 0;
        neighbouringBugCount += spots[pos.row][pos.col + 1] == '#' ? 1 : 0;
      }
      // A bug dies (becoming an empty space) unless there is exactly one
      // bug adjacent to it.
      if (spots[pos.row][pos.col] == '#') {
        if (neighbouringBugCount == 1) {
          newGrid.spots[pos.row][pos.col] = '#';
        } else {
          newGrid.spots[pos.row][pos.col] = '.';
        }
      } else {
        // An empty space becomes infested with a bug if exactly one or
        // two bugs are adjacent to it.
        if (neighbouringBugCount == 2 || neighbouringBugCount == 1) {
          newGrid.spots[pos.row][pos.col] = '#';
        } else {
          newGrid.spots[pos.row][pos.col] = '.';
        }
      }
    }

    // Now evolve the grid on the levels above and below but first create
    // them if they dont exist
    if (grids.containsKey(level - 1)) {
      // An outer level exists. We need to evolve it.
      if (level <= 0) grids[level - 1]!.evolve(grids);
    } else if (hasBugsInOuterRing()) {
      // We have bugs in the outer ring so we need to create an outer level
      // if it doesnt already exist
      // This if statement also stops the recursion since a new outer level
      // only could get bugs in the inner ring
      grids[level - 1] = Grid(5, 5);
      grids[level - 1]!.level = level - 1;
      grids[level - 1]!.evolve(grids);
    }

    if (grids.containsKey(level + 1)) {
      // An inner level exists. We need to evolve it
      if (level >= 0) grids[level + 1]!.evolve(grids);
    } else if (hasBugsInInnerRing()) {
      // We have bugs in the inner ring so we need to create an inner level
      // if it doesnt already exist
      // This if statement also stops the recursion since a new inner level
      // only could get bugs in the outer ring
      grids[level + 1] = Grid(5, 5);
      grids[level + 1]!.level = level + 1;
      grids[level + 1]!.evolve(grids);
    }

    spots = newGrid.spots;
  }

  void show() {
    print('');
    print('Level : ${level}');
    for (int row = 0; row < height; row++) {
      String line = '';
      for (int col = 0; col < width; col++) {
        if (col == 2 && row == 2) {
          line += '?';
        } else {
          line += spots[row][col];
        }
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

  bool hasBugsInOuterRing() {
    for (final pos in outerRing) {
      if (spots[pos.row][pos.col] == '#') return true;
    }
    return false;
  }

  bool hasBugsInInnerRing() {
    for (final pos in innerRing) {
      if (spots[pos.row][pos.col] == '#') return true;
    }
    return false;
  }

  int getLeftmostColumnBugCount() {
    int count = 0;
    for (int row = 0; row < 5; row++) {
      count += spots[row][0] == '#' ? 1 : 0;
    }
    return count;
  }

  int getRightmostColumnBugCount() {
    int count = 0;
    for (int row = 0; row < 5; row++) {
      count += spots[row][4] == '#' ? 1 : 0;
    }
    return count;
  }

  int getTopRowBugCount() {
    int count = 0;
    for (int col = 0; col < 5; col++) {
      count += spots[0][col] == '#' ? 1 : 0;
    }
    return count;
  }

  int getBottomRowBugCount() {
    int count = 0;
    for (int col = 0; col < 5; col++) {
      count += spots[4][col] == '#' ? 1 : 0;
    }
    return count;
  }

  int countBugs() {
    int count = 0;
    for (final pos in outerRing) {
      if (spots[pos.row][pos.col] == '#') count++;
    }
    for (final pos in innerRing) {
      if (spots[pos.row][pos.col] == '#') count++;
    }
    return count;
  }
}
