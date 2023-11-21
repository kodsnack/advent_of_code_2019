import 'dart:collection';

import '../util/linepos.dart';
import '../util/lprange.dart';
import '../util/util.dart';

// const String inputFile = 'day18/example.txt';
const String inputFile = 'day18/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInput(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  // print('Part 2:');
  // final resultP2 = calcResultP2(input);
  // print(resultP2);
}

int calcResultP1(List<String> input) {
  Board board = Board(input);

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

class Board {
  late LinePos playerPos;
  Map<String, LinePos> keys = {};
  Map<String, LinePos> doors = {};
  Set<LinePos> walls = {};
  late LPRange range;

  Board(List<String> lines) {
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

  int calcShortestPath() {
    bool ready = false;

    Queue<PlayerState> stateQueue =
        Queue(); // Queue of possible states to check
    Set<PlayerState> visitedStates = {}; // Set of already visited states
    stateQueue.add(PlayerState(pos: playerPos, stepCount: 0));

    while (stateQueue.isNotEmpty && !ready) {
      final state = stateQueue.removeFirst();
      if (visitedStates.contains(state)) continue; // Skip if we have been here

      if (walls.contains(state.pos)) continue; // Skip if it is a wall

      // Did we hit a door? If we dont have the correct key we skip. Otherwise just move on
      if (doors.containsValue(state.pos)) {
        final neededKey = doors.entries
            .firstWhere((el) => el.value == state.pos)
            .key
            .toLowerCase();
        if (!state.collectedKeys.contains(neededKey)) continue;
      }

      if (keys.containsValue(state.pos)) {
        final foundKey =
            keys.entries.firstWhere((el) => el.value == state.pos).key;
        state.collectedKeys.add(foundKey);
        // Check if we have found this already. In that case we dont need to add it. Since it must have
        // been a shorter path.
        final tempState =
            PlayerState(pos: state.pos, stepCount: state.stepCount);
        tempState.collectedKeys = Set.from(state.collectedKeys);
        if (visitedStates.contains(tempState)) continue;
      }

      ready = state.collectedKeys.length == keys.length;

      visitedStates.add(state);

      for (final neighbourPos in state.pos.getNeighbours()) {
        if (walls.contains(neighbourPos)) continue;
        final newState =
            PlayerState(pos: neighbourPos, stepCount: state.stepCount + 1);
        newState.collectedKeys = Set.from(state.collectedKeys);
        stateQueue.add(newState);
      }
      if (ready) return state.stepCount;
    }
    return -1;
  }

  Map<String, int> getReachable(LinePos pos, int count,
      {required List<String> unlockedDoors, required Set<LinePos> visited}) {
    // Check if pos is outside range
    if (!range.contains(pos)) return {};

    // Check if we have been here already
    if (visited.contains(pos)) return {};

    visited.add(pos);

    // Check if it is a wall.
    if (walls.contains(pos)) return {};

    // Check if it is a locked door
    if (doors.containsValue(pos)) {
      return {};
    }

    // Check if it is a key
    if (keys.containsValue(pos)) {
      final key = keys.keys.singleWhere((p) => keys[p] == pos);
      return {key: count};
    }

    Map<String, int> reachableKeysAndLocks = {};

    final neighbours = pos.getNeighbours();
    for (final neighbour in neighbours) {
      reachableKeysAndLocks.addAll(getReachable(neighbour, count + 1,
          unlockedDoors: unlockedDoors, visited: visited));
    }
    return reachableKeysAndLocks;
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
