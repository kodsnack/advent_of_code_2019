import '../util/pos.dart';
import '../util/range.dart';
import '../util/util.dart';

const String inputFile = 'day11/input.txt';

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
  Map<Pos, int> tiles = {};
  Pos pos = Pos(0, 0);

  int direction = 0; // Up = 0 , right = 1, down = 2, left = 3
  bool ready = false;
  while (!ready) {
    computer.inputQueue.add(colorAtPos(tiles, pos));
    computer.runProgram();
    if (computer.outputQueue.isEmpty) {
      ready = true;
    } else {
      tiles[pos] = computer.outputQueue.removeAt(0);
      int turn = computer.outputQueue.removeAt(0) == 0 ? -1 : 1;

      direction = (direction + turn) % 4;
      pos = pos.moveDir(['U', 'R', 'D', 'L'][direction]);
    }
  }

  return tiles.length;
}

int calcResultP2(String program) {
  IntCodeComputer computer = IntCodeComputer(program);
  Map<Pos, int> tiles = {};
  Pos pos = Pos(0, 0);
  tiles[pos] = 1;

  int direction = 0; // Up = 0 , right = 1, down = 2, left = 3
  bool ready = false;
  while (!ready) {
    computer.inputQueue.add(colorAtPos(tiles, pos));
    computer.runProgram();
    if (computer.outputQueue.isEmpty) {
      ready = true;
    } else {
      tiles[pos] = computer.outputQueue.removeAt(0);
      int turn = computer.outputQueue.removeAt(0) == 0 ? -1 : 1;

      direction = (direction + turn) % 4;
      pos = pos.moveDir(['U', 'R', 'D', 'L'][direction]);
    }
  }

  drawTiles(tiles);

  return 0;
}

void drawTiles(Map<Pos, int> tiles) {
  // Find area with painted tiles
  Range range = Range();
  for (final pos in tiles.keys) {
    range.extend(pos);
  }
  for (int y = range.yMax; y >= range.yMin; y--) {
    String line = '';
    for (int x = range.xMin; x <= range.xMax; x++) {
      line += tiles[Pos(x, y)] == 1 ? '#' : ' ';
    }
    print(line);
  }
}

int colorAtPos(Map<Pos, int> tiles, Pos pos) {
  if (!tiles.containsKey(pos)) return 0;
  return tiles[pos]!;
}

enum ParamMode { position, immediate, relative }

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

  OpResult doInstruction(Map<int, int> memory, int opPtr);

  void setParamMode(int instruction) {
    int paramModeDig = instruction ~/ 10000;
    paramMode[2] = ParamMode.values[paramModeDig];
    // paramMode[2] = paramModeDig > 0 ? ParamMode.immediate : ParamMode.position;

    paramModeDig = instruction % 10000 ~/ 1000;
    paramMode[1] = ParamMode.values[paramModeDig];
    // paramMode[1] = paramModeDig > 0 ? ParamMode.immediate : ParamMode.position;

    paramModeDig = instruction % 1000 ~/ 100;
    paramMode[0] = ParamMode.values[paramModeDig];
    // paramMode[0] = paramModeDig > 0 ? ParamMode.immediate : ParamMode.position;
  }

  int getOperator(Map<int, int> memory, int opPtr, ParamMode paramMode) {
    if (paramMode == ParamMode.immediate) return getMemoryAt(memory, opPtr);
    if (paramMode == ParamMode.relative)
      return getMemoryAt(
          memory, getMemoryAt(memory, opPtr) + computer.relativeBase);
    return getMemoryAt(memory, getMemoryAt(memory, opPtr));
  }

  void storeResult(
      Map<int, int> memory, int result, int opPtr, ParamMode paramMode) {
    if (paramMode == ParamMode.immediate) {
      memory[opPtr] = result;
    }
    if (paramMode == ParamMode.relative) {
      memory[getMemoryAt(memory, opPtr) + computer.relativeBase] = result;
    }
    if (paramMode == ParamMode.position) {
      memory[getMemoryAt(memory, opPtr)] = result;
    }
  }
}

class AddOpCode extends OpCode {
  int instruction;

  @override
  OpResult doInstruction(Map<int, int> memory, int opPtr) {
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
  OpResult doInstruction(Map<int, int> memory, int opPtr) {
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
  OpResult doInstruction(Map<int, int> memory, int opPtr) {
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
  OpResult doInstruction(Map<int, int> memory, int opPtr) {
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
  OpResult doInstruction(Map<int, int> memory, int opPtr) {
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
  OpResult doInstruction(Map<int, int> memory, int opPtr) {
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

class AdjustRelativeBase extends OpCode {
  int instruction;

  @override
  OpResult doInstruction(Map<int, int> memory, int opPtr) {
    int value = getOperator(memory, opPtr + 1, paramMode[0]);
    computer.relativeBase += value;
    return OpResult(false, 2, false);
  }

  AdjustRelativeBase(this.instruction, super.computer) {
    setParamMode(instruction);
  }
}

class InputOpCode extends OpCode {
  int instruction;

  @override
  OpResult doInstruction(Map<int, int> memory, int opPtr) {
    if (computer.inputQueue.isEmpty) {
      return OpResult(false, 0, true);
    }
    int inputValue = computer.inputQueue.removeAt(0);
    storeResult(memory, inputValue, opPtr + 1, paramMode[0]);
    return OpResult(false, 2, false);
  }

  InputOpCode(this.instruction, super.computer) {
    setParamMode(instruction);
  }
}

class OutputOpCode extends OpCode {
  int instruction;
  @override
  OpResult doInstruction(Map<int, int> memory, int opPtr) {
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
  OpResult doInstruction(Map<int, int> memory, int opPtr) {
    return OpResult(false, 0, false); //Return 0 to halt execution
  }

  HaltOpCode(super.computer);
}

class ErrorOpCode extends OpCode {
  @override
  OpResult doInstruction(Map<int, int> memory, int opPtr) {
    throw Exception('Error: Undefined op code');
  }

  ErrorOpCode(super.computer);
}

int getMemoryAt(Map<int, int> memory, int ptr) {
  if (memory.containsKey(ptr)) return memory[ptr]!;
  memory[ptr] = 0;
  return 0;
}

class IntCodeComputer {
  Map<int, int> memory = {};
  List<int> inputQueue = [];
  List<int> outputQueue = [];
  bool halted = false;
  int inputValue = 0;
  int executionPtr = 0;
  int relativeBase = 0;

  IntCodeComputer(String init) {
    List<int> memoryInitList =
        init.split(',').map((e) => int.parse(e)).toList();
    for (int i = 0; i < memoryInitList.length; i++) {
      memory[i] = memoryInitList[i];
    }
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
      case 9:
        return AdjustRelativeBase(instruction, this);
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
      int instruction = getMemoryAt(memory, executionPtr);
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
