import '../util/util.dart';

//const String inputFile = 'day2/example.txt';
const String inputFile = 'day2/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  // print('Part 2:');
  // final resultP2 = calcResultP2(inputLines);
  // print(resultP2);
}

int calcResultP1(String input) {
  var memory = input.split(',').map((e) => int.parse(e)).toList();

  int resultNoun = 0;
  int resultVerb = 0;
  bool ready = false;
  int wantedResult = 19690720;
  while (!ready) {
    for (var noun = 0; noun <= 99; noun++) {
      for (var verb = 0; verb <= 99; verb++) {
        memory = input.split(',').map((e) => int.parse(e)).toList();
        memory[1] = noun;
        memory[2] = verb;
        runProgram(memory);
        if (memory[0] == wantedResult) {
          print('Noun $noun - Verb $verb : ${memory[0]}');
          resultNoun = noun;
          resultVerb = verb;
          ready = true;
          break;
        }
      }
      if (ready) break;
    }
  }
  return resultNoun * 100 + resultVerb;
}

void runProgram(List<int> memory) {
  bool halted = false;
  int opPtr = 0;
  while (!halted) {
    int opCode = memory[opPtr];
    switch (opCode) {
      case 1:
        add(memory, opPtr);
        opPtr += 4;
        break;

      case 2:
        multiply(memory, opPtr);
        opPtr += 4;
        break;

      case 99:
        halted = true;
        break;
      default:
    }
  }
}

void multiply(List<int> memory, int opPtr) {
  int op1 = memory[memory[opPtr + 1]];
  int op2 = memory[memory[opPtr + 2]];
  memory[memory[opPtr + 3]] = op1 * op2;
}

void add(List<int> memory, int opPtr) {
  int op1 = memory[memory[opPtr + 1]];
  int op2 = memory[memory[opPtr + 2]];
  memory[memory[opPtr + 3]] = op1 + op2;
}
