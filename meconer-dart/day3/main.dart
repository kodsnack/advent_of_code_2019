import 'dart:math';

import '../util/pos.dart';
import '../util/util.dart';

//const String inputFile = 'day3/example.txt';
const String inputFile = 'day3/input.txt';

Future<void> main(List<String> args) async {
  var inputLines = await readInput(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(inputLines);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(inputLines);
  print(resultP2);
}

int calcResultP1(List<String> inputLines) {
  Wire wire1 = Wire(inputLines[0]);
  Wire wire2 = Wire(inputLines[1]);
  int closestDist = findClosestDist(wire1, wire2);
  return closestDist;
}

int findClosestDist(Wire wire1, Wire wire2) {
  int shortestManhattanDistance = veryLargeNumber;
  for (final line1 in wire1.lines) {
    for (final line2 in wire2.lines) {
      Pos? intersection = line1.intersection(line2);
      if (intersection != null && intersection != Pos(0, 0)) {
        shortestManhattanDistance = min(shortestManhattanDistance,
            intersection.manhattanDistance(Pos(0, 0)));
      }
    }
  }
  return shortestManhattanDistance;
}

int calcResultP2(List<String> inputLines) {
  Wire wire1 = Wire(inputLines[0]);
  Wire wire2 = Wire(inputLines[1]);
  int fewestSteps = findfewestSteps(wire1, wire2);
  return fewestSteps;
}

int findfewestSteps(Wire wire1, Wire wire2) {
  int fewestSteps = veryLargeNumber;
  int dist1 = 0;
  for (final line1 in wire1.lines) {
    int dist2 = 0;
    for (final line2 in wire2.lines) {
      Pos? intersection = line1.intersection(line2);
      if (intersection != null && intersection != Pos(0, 0)) {
        int steps = dist1 +
            intersection.manhattanDistance(line1.p1) +
            dist2 +
            intersection.manhattanDistance(line2.p1);
        fewestSteps = min(fewestSteps, steps);
      }
      dist2 += line2.p2.manhattanDistance(line2.p1);
    }
    dist1 += line1.p2.manhattanDistance(line1.p1);
  }
  return fewestSteps;
}

class Wire {
  List<Line> lines = [];
  Wire(String moveStr) {
    List<String> movesList = moveStr.split(',');
    Pos start = Pos(0, 0);
    for (final move in movesList) {
      String command = move.substring(0, 1);
      int dist = int.parse(move.substring(1));
      Pos end = start.moveDir(command, dist: dist);
      lines.add(Line(start, end));
      start = end;
    }
  }
}

class Line {
  Pos p1;
  Pos p2;
  Line(this.p1, this.p2);

  bool get isVertical => p1.x == p2.x;
  bool get isHorizontal => p1.y == p2.y;
  int get minX => min(p1.x, p2.x);
  int get maxX => max(p1.x, p2.x);
  int get minY => min(p1.y, p2.y);
  int get maxY => max(p1.y, p2.y);

  Pos? intersection(Line line2) {
    if (isHorizontal) {
      if (line2.isHorizontal) return null;
      if (line2.p1.x < minX || line2.p1.x > maxX) return null;
      if (line2.minY > p1.y || line2.maxY < p1.y) return null;
      return Pos(line2.p1.x, p1.y);
    } else {
      // This line is vertical
      if (line2.isVertical) return null;
      if (line2.p1.y > maxY || line2.p1.y < minY) return null;
      if (line2.minX > p1.x || line2.maxX < p1.x) return null;
      return Pos(p1.x, line2.p1.y);
    }
  }
}
