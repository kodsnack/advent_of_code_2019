import 'dart:collection';

import '../util/intcodecomputer.dart';
import '../util/util.dart';

const String inputFile = 'day19/input.txt';

Future<void> main(List<String> args) async {
  var program = await readInputAsString(inputFile);

  // print('Part 1:');
  // final resultP1 = calcResultP1(program);
  // print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(program);
  print(resultP2);
}

int calcResultP1(String program) {
  IntCodeComputer computer;
  int count = 0;
  for (int y = 0; y <= 49; y++) {
    String line = '';
    for (int x = 0; x <= 49; x++) {
      computer = IntCodeComputer(program);
      computer.addInput(x);
      computer.addInput(y);
      computer.runProgram();
      int result = computer.outputQueue.removeAt(0);
      if (result > 0) count++;
      line += result == 1 ? '#' : '.';
    }
    print(line);
  }
  return count;
}

class BeamLine {
  int lineNo;
  int startCol;
  int endCol;
  BeamLine(this.lineNo, this.startCol, this.endCol);
}

int calcResultP2(String program) {
  IntCodeComputer computer;
  Queue<BeamLine> beamLineQueue = Queue();
  int lineNo = 1000;
  bool addToQ = false;
  int colNo = 0;
  while (true) {
    int startCol = -1;
    int endCol = -1;
    while (endCol < 0) {
      computer = IntCodeComputer(program);
      computer.addInput(colNo);
      computer.addInput(lineNo);
      computer.runProgram();
      int result = computer.outputQueue.removeAt(0);
      if (result == 1 && startCol < 0) startCol = colNo;
      if (startCol > 0) {
        if (result == 0) {
          endCol = colNo;
        }
      }

      colNo++;
    }

    if (endCol - startCol >= 100) addToQ = true;
    if (addToQ) {
      print('$lineNo :  $startCol - $endCol');
      beamLineQueue.add(BeamLine(lineNo, startCol, endCol));
      if (beamLineQueue.length >= 100) {
        final firstLine = beamLineQueue.removeFirst();
        if (startCol <= firstLine.endCol - 100)
          return startCol * 10000 + firstLine.lineNo;
      }
    }
    colNo = startCol - 1;
    lineNo++;
  }
}
