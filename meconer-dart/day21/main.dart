import '../util/intcodecomputer.dart';
import '../util/util.dart';

const String inputFile = 'day21/input.txt';

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
  IntCodeComputer computer;
  computer = IntCodeComputer(program);
  computer.addTextInput('NOT A J');
  computer.addTextInput('NOT B T');
  computer.addTextInput('OR T J');
  computer.addTextInput('NOT C T');
  computer.addTextInput('OR T J');
  computer.addTextInput('AND D J');

  computer.addTextInput('WALK');
  computer.runProgram();
  String line = '';
  while (computer.outputQueue.isNotEmpty) {
    int value = computer.outputQueue.removeAt(0);
    try {
      line += String.fromCharCode(value);
    } catch (e) {
      return value;
    }
  }
  print(line);
  return 0;
}

int calcResultP2(String program) {
  IntCodeComputer computer;
  computer = IntCodeComputer(program);
  // ABCDEFGHI
  //
  computer.addTextInput('NOT A J');
  computer.addTextInput('NOT B T');
  computer.addTextInput('OR T J');
  computer.addTextInput('NOT C T');
  computer.addTextInput('OR T J');
  computer.addTextInput('AND D J');

  computer.addTextInput('NOT I T');
  computer.addTextInput('NOT T T');
  computer.addTextInput('OR F T');
  computer.addTextInput('AND E T');
  computer.addTextInput('OR H T');
  computer.addTextInput('AND T J');

  computer.addTextInput('RUN');
  computer.runProgram();
  String line = '';
  while (computer.outputQueue.isNotEmpty) {
    int value = computer.outputQueue.removeAt(0);
    try {
      line += String.fromCharCode(value);
    } catch (e) {
      return value;
    }
  }
  print(line);
  return 0;
}
