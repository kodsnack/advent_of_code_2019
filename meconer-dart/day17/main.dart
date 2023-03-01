import '../util/intcodecomputer.dart';
import '../util/linepos.dart';
import '../util/util.dart';

const String inputFile = 'day17/input.txt';

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

int calcResultP1(String program) {
  IntCodeComputer computer = IntCodeComputer(program);
  computer.runProgram();
  String s = '';
  int row = 0;
  int col = 0;
  Map<LinePos, String> scaffolds = {};
  for (int charCode in computer.outputQueue) {
    if (charCode == 10) {
      print(s);
      s = '';
      row++;
      col = 0;
    } else {
      final char = String.fromCharCode(charCode);
      s += char;
      if (char == '#') scaffolds[LinePos(col, row)] = char;
      col++;
    }
  }
  print(s);
  int alignmentSum = 0;
  for (final scaffoldPos in scaffolds.keys) {
    final neighbours = scaffoldPos.getNeighbours();
    if (neighbours.every((pos) => scaffolds.containsKey(pos))) {
      int alignment = scaffoldPos.col * scaffoldPos.row;
      alignmentSum += alignment;
    }
  }
  return alignmentSum;
}

int calcResultP2(String program) {
  IntCodeComputer computer = IntCodeComputer(program);

  // Wake up the robot and use overridden movement logic
  computer.memory[0] = 2;

  // Enter main movement routine
  computer.addTextInput('A,B,B,A,C,B,C,C,B,A');

  // Enter movement routine A
  computer.addTextInput('R,10,R,8,L,10,L,10');

  // Enter movement routine B
  computer.addTextInput('R,8,L,6,L,6');

  // Enter movement routine C
  computer.addTextInput('L,10,R,10,L,6');

  // No visual feedback
  computer.addTextInput('n');

  computer.runProgram();
  return computer.outputQueue.last;
}
