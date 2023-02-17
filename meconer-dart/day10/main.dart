import '../util/pos.dart';
import '../util/util.dart';

// const String inputFile = 'day10/example.txt';
const String inputFile = 'day10/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInput(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(input);
  print(resultP2);
}

int calcResultP1(List<String> input) {
  Grid grid = Grid();
  for (final line in input) {
    grid.grid.add(line.split(''));
  }
  grid.grid = grid.grid.reversed.toList();

  int result = grid.getMaxVisibleAsteroids();
  int x = grid.posOfBestLocation.x;
  int y = grid.height - grid.posOfBestLocation.y - 1;
  print('$x, $y');
  // grid.draw();
  return result;
}

int calcResultP2(List<String> input) {
  Grid grid = Grid();
  for (final line in input) {
    grid.grid.add(line.split(''));
  }
  grid.grid = grid.grid.reversed.toList();

  int result = getResultP2(grid, 200);
  // grid.draw();
  return result;
}

int getResultP2(Grid grid, int noToVaporize) {
  grid.getMaxVisibleAsteroids();
  int xOrigin = grid.posOfBestLocation.x;
  int yOrigin = grid.posOfBestLocation.y;
  List<Ray> rayList = [];
  for (int y = 0; y < grid.height; y++) {
    for (var x = 0; x < grid.width; x++) {
      if (x == xOrigin && y == yOrigin) continue;
      if (grid.grid[y][x] == '#') {
        int dY = y - yOrigin;
        int dX = x - xOrigin;
        int dist = dY.abs() + dX.abs();
        int quadrant = getQuadrant(dX, dY);
        double slope = getSlope(quadrant, dX, dY);
        rayList.add(Ray(quadrant, dist, slope, Pos(x, grid.height - y - 1)));
      }
    }
  }
  rayList.sort((a, b) => a.compare(b));

  double slope = 0;
  int nextIdx = 0;
  Pos lastRemoved = Pos(0, 0);
  for (var i = 0; i < noToVaporize; i++) {
    lastRemoved = rayList[nextIdx].pos;
    rayList.removeAt(nextIdx);
    while (rayList[nextIdx].slope == slope) {
      nextIdx++;
    }
    slope = rayList[nextIdx].slope;
  }
  return lastRemoved.x * 100 + lastRemoved.y;
}

class Grid {
  List<List<String>> grid = [];

  int get width => grid[0].length;
  int get height => grid.length;
  Pos posOfBestLocation = Pos(-1, -1);

  void draw() {
    print('==========');
    for (final line in grid.reversed) {
      String lineToPrint = '';
      for (final char in line) {
        lineToPrint += char;
      }
      print(lineToPrint);
    }
    print('==========');
  }

  int getMaxVisibleAsteroids() {
    int max = 0;
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        if (grid[y][x] == '#') {
          int visibleAsteroidCount = countVisible(x, y);
          //print('$x, $y : $visibleAsteroidCount');
          if (visibleAsteroidCount > max) {
            max = visibleAsteroidCount;
            posOfBestLocation = Pos(x, y);
          }
        }
      }
    }
    return max;
  }

  int countVisible(int orgX, int orgY) {
    Set<Pos> visiblePositions = {};
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
        if (x == orgX && y == orgY) continue;
        if (grid[y][x] == '.') continue;
        if (isVisible(x, y, orgX, orgY)) visiblePositions.add(Pos(x, y));
      }
    }
    return visiblePositions.length;
  }

  bool isVisible(int x, int y, int orgX, int orgY) {
    int dX = x - orgX;
    int dY = y - orgY;
    int gcd = 1;
    if (dX == 0) {
      dY = dY.sign;
    } else if (dY == 0) {
      dX = dX.sign;
    } else {
      gcd = findGcd(dX.abs(), dY.abs());
    }
    int slopeX = dX ~/ gcd;
    int slopeY = dY ~/ gcd;
    int xToCheck = orgX + slopeX;
    int yToCheck = orgY + slopeY;
    while (x != xToCheck || y != yToCheck) {
      if (grid[yToCheck][xToCheck] == '#') return false;
      xToCheck += slopeX;
      yToCheck += slopeY;
    }
    return true;
  }
}

double getSlope(int quadrant, int dX, int dY) {
  dX = dX.abs();
  dY = dY.abs();
  switch (quadrant) {
    case 0:
    case 2:
      return dX / dY;
    case 1:
    case 3:
      return dY / dX;
    default:
      return 0;
  }
}

int getQuadrant(int dX, int dY) {
  if (dX >= 0 && dY > 0) return 0;
  if (dX > 0 && dY <= 0) return 1;
  if (dX <= 0 && dY < 0) return 2;
  if (dX < 0 && dY >= 0) return 3;
  return -1;
}

class Ray {
  int quadrant; // 0 for NE, 1 for SE, 2 for SW and 3 for NW so we can sort the rays
  int dist; // Manhattandistance for asteroid
  double
      slope; // Slope counted from the start of the quadrant and clockwise. Also for sorting
  Pos pos; // Position coordinates in x/y system
  Ray(this.quadrant, this.dist, this.slope, this.pos);

  int compare(Ray otherRay) {
    if (quadrant < otherRay.quadrant) return -1;
    if (quadrant > otherRay.quadrant) return 1;
    if (slope < otherRay.slope) return -1;
    if (slope > otherRay.slope) return 1;
    if (dist < otherRay.dist) return -1;
    if (dist > otherRay.dist) return 1;
    return 0;
  }
}
