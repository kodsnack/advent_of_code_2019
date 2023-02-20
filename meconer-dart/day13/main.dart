import '../util/intcodecomputer.dart';
import '../util/pos.dart';
import '../util/range.dart';
import '../util/util.dart';

const String inputFile = 'day13/input.txt';

Future<void> main(List<String> args) async {
  var program = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(program);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(program);
  print(resultP2);
}

int calcResultP1(String program) {
  IntCodeComputer computer = IntCodeComputer(program);
  computer.runProgram();
  Map<Pos, int> tiles = {};
  while (!computer.outputQueue.isEmpty) {
    int x = computer.outputQueue.removeAt(0);
    int y = computer.outputQueue.removeAt(0);
    int tileId = computer.outputQueue.removeAt(0);

    Pos pos = Pos(x, y);
    if (tiles.containsKey(pos)) {
      if (tileId == 4 && tiles[pos] == 2) tiles[pos] = tileId;
    }
    tiles[Pos(x, y)] = tileId;
  }
  int count = 0;
  for (var tile in tiles.values) {
    if (tile == 2) count++;
  }

  return count;
}

int calcResultP2(String program) {
  IntCodeComputer computer = IntCodeComputer(program);
  computer.memory[0] = 2;

  bool ready = false;
  Map<Pos, int> tiles = {};
  int score = 0;
  Range range = Range();
  bool foundRange = false;

  while (!ready) {
    computer.runProgram();
    int paddleX = 0;
    int ballX = 0;

    while (!computer.outputQueue.isEmpty) {
      int x = computer.outputQueue.removeAt(0);
      int y = computer.outputQueue.removeAt(0);
      int tileId = computer.outputQueue.removeAt(0);
      if (x < 0) {
        score = tileId;
        continue;
      }
      Pos pos = Pos(x, y);
      if (!foundRange) range.extend(pos);
      if (tiles.containsKey(pos)) {
        if (tileId == 4 && tiles[pos] == 2) tiles[pos] = tileId;
      }
      if (tileId == 3) paddleX = x;
      if (tileId == 4) ballX = x;
      tiles[Pos(x, y)] = tileId;
    }
    foundRange = true;
    // drawTiles(tiles, range);
    computer.inputQueue.add((ballX - paddleX).sign);
    int count = countBlocks(tiles);
    ready = count == 0;
  }
  return score;
}

void drawTiles(Map<Pos, int> tiles, Range range) {
  for (int y = range.yMax; y >= range.yMin; y--) {
    String line = '';
    for (int x = range.xMin; x <= range.xMax; x++) {
      line += [' ', '#', 'X', '_', 'o'][tiles[Pos(x, y)]!];
    }
    print(line);
  }
}

int countBlocks(Map<Pos, int> tiles) {
  int count = 0;

  for (var tile in tiles.values) {
    if (tile == 2) count++;
  }
  return count;
}
