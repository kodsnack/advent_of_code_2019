import 'dart:collection';

import '../util/linepos.dart';
import '../util/lprange.dart';
import '../util/util.dart';

// const String inputFile = 'day18/example2.txt';
const String inputFile = 'day18/input2.txt';

Future<void> main(List<String> args) async {
  var input = await readInput(inputFile);

  print('Part 2:');
  final resultP2 = calcResultP2(input);
  print(resultP2);
}

int calcResultP2(List<String> input) {
  Board2 board = Board2(input);

  int result = board.calcShortestPath();
  return result;
}

class PlayerState {
  Set<String> collectedKeys = {};
  LinePos pos;
  int stepCount;
  PlayerState({required LinePos this.pos, required int this.stepCount});

  @override
  bool operator ==(Object other) {
    if (!(other is PlayerState)) return false;

    if (!this.collectedKeys.containsAll(other.collectedKeys)) return false;
    if (!other.collectedKeys.containsAll(this.collectedKeys)) return false;
    if (this.pos != other.pos) return false;
    return true;
  }

  @override
  int get hashCode {
    var keys = collectedKeys.toList();
    keys.sort();
    return pos.hashCode + keys.join().hashCode;
  }

  void printState() {
    print('$stepCount  : ${pos.col},${pos.row} : ${collectedKeys.join()}');
  }
}

class BoardPart {
  late LinePos playerPos;
  Map<String, LinePos> keys = {};
  Map<String, LinePos> doors = {};
  Set<LinePos> walls = {};
  late LPRange range;

  BoardPart(List<String> lines) {
    int row = 0;
    range = LPRange();
    range.extend(LinePos(0, 0));
    range.extend(LinePos(lines[0].length - 1, lines.length - 1));
    for (final line in lines) {
      List<String> gridLine = line.split('');
      for (int col = 0; col < gridLine.length; col++) {
        String char = gridLine[col];
        if (char == '@') playerPos = LinePos(col, row);
        if (isKey(char)) keys[char] = LinePos(col, row);
        if (isDoor(char)) doors[char] = LinePos(col, row);
        if (isWall(char)) walls.add(LinePos(col, row));
      }
      row++;
    }
  }

  int calcShortestPath(Set<String> globallyFoundKeys) {
    Queue<PlayerState> stateQueue =
        Queue(); // Queue of possible states to check
    Set<PlayerState> visitedStates = {}; // Set of already visited states
    final startState = PlayerState(pos: playerPos, stepCount: 0);
    stateQueue.add(startState);

    while (stateQueue.isNotEmpty) {
      final state = stateQueue.removeFirst();
      if (visitedStates.contains(state)) continue; // Skip if we have been here

      // Check if we found a key
      if (keys.containsValue(state.pos)) {
        final foundKey =
            keys.entries.firstWhere((el) => el.value == state.pos).key;

        state.collectedKeys.add(foundKey);

        // Exit if we found all keys
        if (state.collectedKeys.length == keys.length) return state.stepCount;
      }
      if (visitedStates.contains(state)) continue;
      visitedStates.add(state);

      for (final neighbourPos in state.pos.getNeighbours()) {
        if (walls.contains(neighbourPos)) continue;

        final newState =
            PlayerState(pos: neighbourPos, stepCount: state.stepCount + 1);
        newState.collectedKeys = Set.from(state.collectedKeys);
        stateQueue.add(newState);
      }
    }
    return -1;
  }
}

class Board2 {
  // Map<String, LinePos> keys = {};
  // late LPRange range;
  late List<BoardPart> boardParts;
  Set<String> globallyFoundKeys = {};

  Board2(List<String> lines) {
    boardParts = [];
    List<String> leftLineHalves = [];
    List<String> rightLineHalves = [];
    int lineLength = lines[0].length;
    int midPoint = lineLength ~/ 2;
    for (int lineNo = 0; lineNo <= lines.length ~/ 2; lineNo++) {
      leftLineHalves.add(lines[lineNo].substring(0, midPoint + 1));
      rightLineHalves.add(lines[lineNo].substring(midPoint, lineLength));
    }
    boardParts.add(BoardPart(leftLineHalves));
    boardParts.add(BoardPart(rightLineHalves));

    leftLineHalves = [];
    rightLineHalves = [];
    for (int lineNo = lines.length ~/ 2; lineNo < lines.length; lineNo++) {
      leftLineHalves.add(lines[lineNo].substring(0, midPoint + 1));
      rightLineHalves.add(lines[lineNo].substring(midPoint, lineLength));
    }
    boardParts.add(BoardPart(leftLineHalves));
    boardParts.add(BoardPart(rightLineHalves));
  }

  int calcShortestPath() {
    int currentBoardNo = 0;
    int noOfSteps = 0;
    for (int boardNo = 0; boardNo < 4; boardNo++) {
      int partSteps =
          boardParts[currentBoardNo].calcShortestPath(globallyFoundKeys);
      if (partSteps > 0) noOfSteps += partSteps;
      currentBoardNo++;
      currentBoardNo %= 4;
    }
    return noOfSteps;
  }
}

// return true if this is a key (Lowercase char)
bool isKey(String char) {
  int code = char.codeUnitAt(0);
  return code >= 'a'.codeUnitAt(0) && code <= 'z'.codeUnitAt(0);
}

// return true if this is a door (Uppercase char)
bool isDoor(String char) {
  int code = char.codeUnitAt(0);
  return code >= 'A'.codeUnitAt(0) && code <= 'Z'.codeUnitAt(0);
}

// return true if this is a wall (#)
bool isWall(String char) {
  return char == '#';
}
