import '../util/util.dart';

//const String inputFile = 'day5/example.txt';
const String inputFile = 'day5/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResult(input);
  print(resultP1);
}

int calcResult(String input) {
  IntCodeComputer computer = IntCodeComputer(input);
  computer.inputValue = 5;
  computer.runProgram();
  return 0;
}

enum ParamMode { position, immediate }

class OpPtrResult {
  bool absolute;
  int value;
  OpPtrResult(this.absolute, this.value);
}

abstract class OpCode {
  //int instruction = 0;
  late List<ParamMode> paramMode;

  OpCode() {
    paramMode = List.generate(3, (_) => ParamMode.position);
  }

  OpPtrResult doInstruction(List<int> memory, int opPtr);

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
  OpPtrResult doInstruction(List<int> memory, int opPtr) {
    int op1 = getOperator(memory, opPtr + 1, paramMode[0]);
    int op2 = getOperator(memory, opPtr + 2, paramMode[1]);
    int result = op1 + op2;
    storeResult(memory, result, opPtr + 3, paramMode[2]);

    return OpPtrResult(false, 4);
  }

  AddOpCode(this.instruction) {
    setParamMode(instruction);
  }
}

class MultiplyOpCode extends OpCode {
  int instruction;

  @override
  OpPtrResult doInstruction(List<int> memory, int opPtr) {
    int op1 = getOperator(memory, opPtr + 1, paramMode[0]);
    int op2 = getOperator(memory, opPtr + 2, paramMode[1]);
    int result = op1 * op2;
    storeResult(memory, result, opPtr + 3, paramMode[2]);

    return OpPtrResult(false, 4);
  }

  MultiplyOpCode(this.instruction) {
    setParamMode(instruction);
  }
}

class JumpIfTrue extends OpCode {
  int instruction;

  @override
  OpPtrResult doInstruction(List<int> memory, int opPtr) {
    int op1 = getOperator(memory, opPtr + 1, paramMode[0]);
    int op2 = getOperator(memory, opPtr + 2, paramMode[1]);
    if (op1 != 0) return (OpPtrResult(true, op2));
    return OpPtrResult(false, 3);
  }

  JumpIfTrue(this.instruction) {
    setParamMode(instruction);
  }
}

class JumpIfFalse extends OpCode {
  int instruction;

  @override
  OpPtrResult doInstruction(List<int> memory, int opPtr) {
    int op1 = getOperator(memory, opPtr + 1, paramMode[0]);
    int op2 = getOperator(memory, opPtr + 2, paramMode[1]);
    if (op1 == 0) return (OpPtrResult(true, op2));
    return OpPtrResult(false, 3);
  }

  JumpIfFalse(this.instruction) {
    setParamMode(instruction);
  }
}

class LessThan extends OpCode {
  int instruction;

  @override
  OpPtrResult doInstruction(List<int> memory, int opPtr) {
    int op1 = getOperator(memory, opPtr + 1, paramMode[0]);
    int op2 = getOperator(memory, opPtr + 2, paramMode[1]);
    int result = op1 < op2 ? 1 : 0;
    storeResult(memory, result, opPtr + 3, paramMode[2]);

    return OpPtrResult(false, 4);
  }

  LessThan(this.instruction) {
    setParamMode(instruction);
  }
}

class Equals extends OpCode {
  int instruction;

  @override
  OpPtrResult doInstruction(List<int> memory, int opPtr) {
    int op1 = getOperator(memory, opPtr + 1, paramMode[0]);
    int op2 = getOperator(memory, opPtr + 2, paramMode[1]);
    int result = op1 == op2 ? 1 : 0;
    storeResult(memory, result, opPtr + 3, paramMode[2]);

    return OpPtrResult(false, 4);
  }

  Equals(this.instruction) {
    setParamMode(instruction);
  }
}

class InputOpCode extends OpCode {
  int instruction;
  int inputValue;
  @override
  OpPtrResult doInstruction(List<int> memory, int opPtr) {
    storeResult(memory, inputValue, opPtr + 1, paramMode[1]);
    return OpPtrResult(false, 2);
  }

  InputOpCode(this.instruction, this.inputValue) {
    setParamMode(instruction);
  }
}

class OutputOpCode extends OpCode {
  int instruction;
  @override
  OpPtrResult doInstruction(List<int> memory, int opPtr) {
    int value = getOperator(memory, opPtr + 1, paramMode[0]);
    print('Output : $value');
    return OpPtrResult(false, 2);
  }

  OutputOpCode(this.instruction) {
    setParamMode(instruction);
  }
}

class HaltOpCode extends OpCode {
  @override
  OpPtrResult doInstruction(List<int> memory, int opPtr) {
    return OpPtrResult(false, 0); //Return 0 to halt execution
  }
}

class ErrorOpCode extends OpCode {
  @override
  OpPtrResult doInstruction(List<int> memory, int opPtr) {
    throw Exception('Error: Undefined op code');
  }
}

class IntCodeComputer {
  List<int> memory = [];
  bool halted = false;
  int inputValue = 0;

  IntCodeComputer(String init) {
    memory = init.split(',').map((e) => int.parse(e)).toList();
  }

  OpCode selectOp(int instruction) {
    int opCode =
        instruction % 100; // Remove mode part and keep only the last two digits
    switch (opCode) {
      case 1:
        return AddOpCode(instruction);
      case 2:
        return MultiplyOpCode(instruction);
      case 3:
        return InputOpCode(instruction, inputValue);
      case 4:
        return OutputOpCode(instruction);
      case 5:
        return JumpIfTrue(instruction);
      case 6:
        return JumpIfFalse(instruction);
      case 7:
        return LessThan(instruction);
      case 8:
        return Equals(instruction);
      case 99:
        return HaltOpCode();
      default:
        return ErrorOpCode();
    }
  }

  void runProgram() {
    bool halted = false;
    int opPtr = 0;
    while (!halted) {
      int instruction = memory[opPtr];
      final op = selectOp(instruction);
      final opPtrResult = op.doInstruction(memory, opPtr);

      if (!opPtrResult.absolute && opPtrResult.value == 0) halted = true;
      if (opPtrResult.absolute) {
        opPtr = opPtrResult.value;
      } else {
        opPtr += opPtrResult.value;
      }
    }
  }
}
