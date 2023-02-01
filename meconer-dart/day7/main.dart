import 'dart:math';

import '../util/util.dart';

//const String inputFile = 'day7/example.txt';
const String inputFile = 'day7/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInputAsString(inputFile);

  // print('Part 1:');
  // final resultP1 = calcResultP1(input);
  // print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(input);
  print(resultP2);
}

int calcResultP1(String program) {
  // Generate a list of all different phase settings with Heaps algorithm
  List<List<int>> phaseSettings =
      getPermutations(List.generate(5, (index) => index));

  // List of computers
  List<IntCodeComputer> computers = [];

  int maxOutput = 0;
  // Test all different phasesetting combinations.
  for (var phaseSetting in phaseSettings) {
    int amplifierSignal = 0;

    List<bool> computerHasFinished = List.filled(5, false);
    computers = [];
    // Init all computers and add phase settings to their input queues
    for (int computerNo = 0; computerNo < 5; computerNo++) {
      IntCodeComputer computer = IntCodeComputer(program);
      computer.inputQueue = [phaseSetting[computerNo]];
      computers.add(computer);
    }

    // Add the amplifier input to the first computer
    computers[0].inputQueue.add(amplifierSignal);

    // Run all the computers and set the output from one computer as the input
    // to the next
    int computerNo = 0;
    while (computerHasFinished.any((finished) => !finished)) {
      bool finished = computers[computerNo].runProgram();
      computerHasFinished[computerNo] = finished;
      amplifierSignal = computers[computerNo].outputQueue.first;
      computerNo = (computerNo + 1) % 5;
      computers[computerNo].inputQueue.add(amplifierSignal);
    }
    maxOutput = max(maxOutput, amplifierSignal);
  }

  return maxOutput;
}

int calcResultP2(String program) {
  // Generate a list of all different phase settings with Heaps algorithm
  List<List<int>> phaseSettings =
      getPermutations(List.generate(5, (index) => index + 5));

  // List of computers
  List<IntCodeComputer> computers = [];

  int maxOutput = 0;
  // Test all different phasesetting combinations.
  for (var phaseSetting in phaseSettings) {
    int amplifierSignal = 0;

    List<bool> computerHasFinished = List.filled(5, false);
    computers = [];
    // Init all computers and add phase settings to their input queues
    for (int computerNo = 0; computerNo < 5; computerNo++) {
      IntCodeComputer computer = IntCodeComputer(program);
      computer.inputQueue = [phaseSetting[computerNo]];
      computers.add(computer);
    }

    // Add the amplifier input to the first computer
    computers[0].inputQueue.add(amplifierSignal);

    // Run all the computers and set the output from one computer as the input
    // to the next
    int computerNo = 0;
    while (computerHasFinished.any((finished) => !finished)) {
      bool finished = computers[computerNo].runProgram();
      computerHasFinished[computerNo] = finished;
      amplifierSignal = computers[computerNo].outputQueue.removeAt(0);
      computerNo = (computerNo + 1) % 5;
      computers[computerNo].inputQueue.add(amplifierSignal);
    }
    maxOutput = max(maxOutput, amplifierSignal);
  }

  return maxOutput;
}

enum ParamMode { position, immediate }

class OpResult {
  bool absolute;
  int value;
  bool waitingForInput;
  OpResult(this.absolute, this.value, this.waitingForInput);
}

abstract class OpCode {
  //int instruction = 0;
  late List<ParamMode> paramMode;
  IntCodeComputer computer;

  OpCode(this.computer) {
    paramMode = List.generate(3, (_) => ParamMode.position);
  }

  OpResult doInstruction(List<int> memory, int opPtr);

  void setParamMode(int instruction) {
    int paramModeDig = instruction ~/ 10000;
    paramMode[2] = paramModeDig > 0 ? ParamMode.immediate : ParamMode.position;

    paramModeDig = instruction % 10000 ~/ 1000;
    paramMode[1] = paramModeDig > 0 ? ParamMode.immediate : ParamMode.position;

    paramModeDig = instruction % 1000 ~/ 100;
    paramMode[0] = paramModeDig > 0 ? ParamMode.immediate : ParamMode.position;
  }

  int getOperator(List<int> memory, int opPtr, ParamMode paramMode) {
    if (paramMode == ParamMode.immediate) return memory[opPtr];
    return memory[memory[opPtr]];
  }

  void storeResult(
      List<int> memory, int result, int opPtr, ParamMode paramMode) {
    if (paramMode == ParamMode.immediate) {
      memory[opPtr] = result;
    }
    if (paramMode == ParamMode.position) {
      memory[memory[opPtr]] = result;
    }
  }
}

class AddOpCode extends OpCode {
  int instruction;

  @override
  OpResult doInstruction(List<int> memory, int opPtr) {
    int op1 = getOperator(memory, opPtr + 1, paramMode[0]);
    int op2 = getOperator(memory, opPtr + 2, paramMode[1]);
    int result = op1 + op2;
    storeResult(memory, result, opPtr + 3, paramMode[2]);

    return OpResult(false, 4, false);
  }

  AddOpCode(this.instruction, super.computer) {
    setParamMode(instruction);
  }
}

class MultiplyOpCode extends OpCode {
  int instruction;

  @override
  OpResult doInstruction(List<int> memory, int opPtr) {
    int op1 = getOperator(memory, opPtr + 1, paramMode[0]);
    int op2 = getOperator(memory, opPtr + 2, paramMode[1]);
    int result = op1 * op2;
    storeResult(memory, result, opPtr + 3, paramMode[2]);

    return OpResult(false, 4, false);
  }

  MultiplyOpCode(this.instruction, super.computer) {
    setParamMode(instruction);
  }
}

class JumpIfTrue extends OpCode {
  int instruction;

  @override
  OpResult doInstruction(List<int> memory, int opPtr) {
    int op1 = getOperator(memory, opPtr + 1, paramMode[0]);
    int op2 = getOperator(memory, opPtr + 2, paramMode[1]);
    if (op1 != 0) return (OpResult(true, op2, false));
    return OpResult(false, 3, false);
  }

  JumpIfTrue(this.instruction, super.computer) {
    setParamMode(instruction);
  }
}

class JumpIfFalse extends OpCode {
  int instruction;

  @override
  OpResult doInstruction(List<int> memory, int opPtr) {
    int op1 = getOperator(memory, opPtr + 1, paramMode[0]);
    int op2 = getOperator(memory, opPtr + 2, paramMode[1]);
    if (op1 == 0) return (OpResult(true, op2, false));
    return OpResult(false, 3, false);
  }

  JumpIfFalse(this.instruction, super.computer) {
    setParamMode(instruction);
  }
}

class LessThan extends OpCode {
  int instruction;

  @override
  OpResult doInstruction(List<int> memory, int opPtr) {
    int op1 = getOperator(memory, opPtr + 1, paramMode[0]);
    int op2 = getOperator(memory, opPtr + 2, paramMode[1]);
    int result = op1 < op2 ? 1 : 0;
    storeResult(memory, result, opPtr + 3, paramMode[2]);

    return OpResult(false, 4, false);
  }

  LessThan(this.instruction, super.computer) {
    setParamMode(instruction);
  }
}

class Equals extends OpCode {
  int instruction;

  @override
  OpResult doInstruction(List<int> memory, int opPtr) {
    int op1 = getOperator(memory, opPtr + 1, paramMode[0]);
    int op2 = getOperator(memory, opPtr + 2, paramMode[1]);
    int result = op1 == op2 ? 1 : 0;
    storeResult(memory, result, opPtr + 3, paramMode[2]);

    return OpResult(false, 4, false);
  }

  Equals(this.instruction, super.computer) {
    setParamMode(instruction);
  }
}

class InputOpCode extends OpCode {
  int instruction;

  @override
  OpResult doInstruction(List<int> memory, int opPtr) {
    if (computer.inputQueue.isEmpty) {
      return OpResult(false, 0, true);
    }
    int inputValue = computer.inputQueue.removeAt(0);
    storeResult(memory, inputValue, opPtr + 1, paramMode[1]);
    return OpResult(false, 2, false);
  }

  InputOpCode(this.instruction, super.computer) {
    setParamMode(instruction);
  }
}

class OutputOpCode extends OpCode {
  int instruction;
  @override
  OpResult doInstruction(List<int> memory, int opPtr) {
    int value = getOperator(memory, opPtr + 1, paramMode[0]);
    //print('Output : $value');
    computer.outputQueue.add(value);
    return OpResult(false, 2, false);
  }

  OutputOpCode(this.instruction, super.computer) {
    setParamMode(instruction);
  }
}

class HaltOpCode extends OpCode {
  @override
  OpResult doInstruction(List<int> memory, int opPtr) {
    return OpResult(false, 0, false); //Return 0 to halt execution
  }

  HaltOpCode(super.computer);
}

class ErrorOpCode extends OpCode {
  @override
  OpResult doInstruction(List<int> memory, int opPtr) {
    throw Exception('Error: Undefined op code');
  }

  ErrorOpCode(super.computer);
}

class IntCodeComputer {
  List<int> memory = [];
  List<int> inputQueue = [];
  List<int> outputQueue = [];
  bool halted = false;
  int inputValue = 0;
  int executionPtr = 0;

  IntCodeComputer(String init) {
    memory = init.split(',').map((e) => int.parse(e)).toList();
    executionPtr = 0;
  }

  OpCode selectOp(int instruction) {
    int opCode =
        instruction % 100; // Remove mode part and keep only the last two digits
    switch (opCode) {
      case 1:
        return AddOpCode(instruction, this);
      case 2:
        return MultiplyOpCode(instruction, this);
      case 3:
        return InputOpCode(instruction, this);
      case 4:
        return OutputOpCode(instruction, this);
      case 5:
        return JumpIfTrue(instruction, this);
      case 6:
        return JumpIfFalse(instruction, this);
      case 7:
        return LessThan(instruction, this);
      case 8:
        return Equals(instruction, this);
      case 99:
        return HaltOpCode(this);
      default:
        return ErrorOpCode(this);
    }
  }

  bool runProgram() {
    bool halted = false;

    bool waitingForInput = false;
    while (!halted && !waitingForInput) {
      int instruction = memory[executionPtr];
      final op = selectOp(instruction);
      final opResult = op.doInstruction(memory, executionPtr);

      if (opResult.waitingForInput) {
        waitingForInput = true;
        break;
      }
      if (!opResult.absolute && opResult.value == 0) halted = true;
      if (opResult.absolute) {
        executionPtr = opResult.value;
      } else {
        executionPtr += opResult.value;
      }
    }
    return halted;
  }
}
