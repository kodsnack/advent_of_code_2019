import '../util/intcodecomputer.dart';
import '../util/pos.dart';
import '../util/range.dart';
import '../util/util.dart';

const String inputFile = 'day15/input.txt';

Future<void> main(List<String> args) async {
  var program = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(program);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(program);
  print(resultP2);
}

enum Direction {
  north(1),
  south(2),
  west(3),
  east(4);

  const Direction(this.value);
  final int value;
  Direction get oppositeDirection => [
        Direction.south,
        Direction.north,
        Direction.east,
        Direction.west
      ][this.value - 1];
}

enum Space {
  wall(0),
  empty(1),
  oxygen(2);

  const Space(this.value);
  final int value;
  String get char => ['#', '_', 'O'][this.value];
}

Pos goalPos = Pos(0, 0);
bool foundGoal = false;

int calcResultP1(String program) {
  Map<Pos, Space> knownPositions = {};
  Map<Pos, int> distancesFromStart = {};

  IntCodeComputer computer = IntCodeComputer(program);
  // Start position
  Pos pos = Pos(0, 0);
  distancesFromStart[pos] = 0;
  knownPositions[pos] = Space.empty;

  // Check all four neighbours
  for (final direction in Direction.values) {
    exploreDirection(direction, pos, computer, distancesFromStart[pos]! + 1,
        distancesFromStart, knownPositions);
  }
  drawPositions(knownPositions);
  return distancesFromStart[goalPos]!;
}

int calcResultP2(String program) {
  Map<Pos, Space> knownPositions = {};
  Map<Pos, int> distancesFromStart = {};

  IntCodeComputer computer = IntCodeComputer(program);
  // Start position
  Pos pos = Pos(0, 0);
  distancesFromStart[pos] = 0;
  knownPositions[pos] = Space.empty;

  // Explore the map
  for (final direction in Direction.values) {
    exploreDirection(direction, pos, computer, distancesFromStart[pos]! + 1,
        distancesFromStart, knownPositions);
  }
  drawPositions(knownPositions);

  // Fill all space with oxygen
  Set<Pos> oxygenPositions = {goalPos};
  int time = 0;
  bool allFilled = false;
  while (!allFilled) {
    Set<Pos> newOxyPositions = {};
    for (final pos in oxygenPositions) {
      for (final neighbourPos in pos.getNeighbours()) {
        if (knownPositions[neighbourPos]! == Space.empty) {
          knownPositions[neighbourPos] = Space.oxygen;
          newOxyPositions.add(neighbourPos);
        }
      }
    }
    allFilled = newOxyPositions.isEmpty;
    oxygenPositions.addAll(newOxyPositions);
    // drawPositions(knownPositions);
    time++;
  }

  return time - 1;
}

void exploreDirection(
    Direction direction,
    Pos pos,
    IntCodeComputer computer,
    int distanceFromStart,
    Map<Pos, int> distancesFromStart,
    Map<Pos, Space> knownPositions) {
  IntCodeComputer nextComputer = IntCodeComputer.from(computer);
  nextComputer.inputQueue.add(direction.value);
  nextComputer.runProgram();
  Space result = Space.values[nextComputer.outputQueue.removeAt(0)];
  Pos nextPos = moveDirection(pos, direction);

  knownPositions[nextPos] = result;
  // A wall. Just return
  if (result == Space.wall) return;

  if (distancesFromStart.containsKey(nextPos)) {
    // We have been here before. Check if we have a shorter dist now.
    // If not just return
    if (distanceFromStart >= distancesFromStart[nextPos]!) return;
    // Otherwise continue to explore
  }

  distancesFromStart[nextPos] = distanceFromStart;

  if (result == Space.oxygen) {
    // We found the oxygen station
    foundGoal = true;
    goalPos = nextPos;
  }

  //drawPositions(knownPositions);
  for (final nextDirection in Direction.values) {
    exploreDirection(nextDirection, nextPos, nextComputer,
        distanceFromStart + 1, distancesFromStart, knownPositions);
  }
}

Pos moveDirection(Pos pos, Direction direction) {
  switch (direction) {
    case Direction.north:
      return Pos(pos.x, pos.y + 1);
    case Direction.south:
      return Pos(pos.x, pos.y - 1);
    case Direction.west:
      return Pos(pos.x - 1, pos.y);
    case Direction.east:
      return Pos(pos.x + 1, pos.y);
    default:
      throw Exception('Illegal direction');
  }
}

void drawPositions(Map<Pos, Space> knownPositions) {
  Range range = Range();
  for (final pos in knownPositions.keys) {
    range.extend(pos);
  }
  for (int y = range.yMax + 1; y >= range.yMin - 1; y--) {
    String line = '';
    for (int x = range.xMin - 1; x <= range.xMax + 1; x++) {
      if (knownPositions.containsKey(Pos(x, y))) {
        line += knownPositions[Pos(x, y)]!.char;
      } else {
        line += ' ';
      }
    }
    print(line);
  }
  print('');
}
