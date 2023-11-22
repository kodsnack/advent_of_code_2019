import '../util/linepos.dart';
import '../util/util.dart';

// const String inputFile = 'day20/example.txt';
const String inputFile = 'day20/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(input);
  print(resultP2);
}

int calcResultP1(String input) {
  Grid grid = Grid(input);
  grid.findStartPt();
  grid.findEndPt();
  grid.findWarps();
  int result = grid.findShortestPath();
  return result;
}

int calcResultP2(String input) {
  Grid grid = Grid(input);
  grid.findStartPt();
  grid.findEndPt();
  grid.findWarps();
  int result = grid.findShortestPath2();
  return result;
}

class Warp {
  LinePos? p1;
  LinePos? p2;
}

class Grid {
  late LinePos startPoint;
  late LinePos endPoint;

  late List<String> lines;
  Map<String, Warp> warps = {};

  Grid(String input) {
    lines = input.split('\n');
  }

  void findStartPt() {
    for (var row = 0; row < lines.length; row++) {
      for (int col = 0; col < lines[0].length; col++) {
        final c = getChar(row, col);
        if (c == 'A') {
          var cD = getChar(row + 1, col);
          if (cD == 'A') {
            if (getChar(row + 2, col) == '.') {
              startPoint = LinePos(col, row + 2);
              return;
            }
            if (getChar(row - 1, col) == '.') {
              startPoint = LinePos(col, row - 1);
              return;
            }
          }
        }
      }
    }
  }

  void findEndPt() {
    for (var row = 0; row < lines.length; row++) {
      for (int col = 0; col < lines[0].length; col++) {
        final c = getChar(row, col);
        if (c == 'Z') {
          var cD = getChar(row + 1, col);
          if (cD == 'Z') {
            if (getChar(row + 2, col) == '.') {
              endPoint = LinePos(col, row + 2);
              return;
            }
            if (getChar(row - 1, col) == '.') {
              endPoint = LinePos(col, row - 1);
              return;
            }
          }
          cD = getChar(row, col + 1);
          if (cD == 'Z') {
            if (getChar(row, col + 2) == '.') {
              endPoint = LinePos(col + 2, row);
              return;
            }
            if (getChar(row, col - 1) == '.') {
              endPoint = LinePos(col - 1, row);
              return;
            }
          }
        }
      }
    }
  }

  void findWarps() {
    for (var row = 0; row < lines.length; row++) {
      for (int col = 0; col < lines[0].length; col++) {
        final c = getChar(row, col);
        if (isAlpha(c)) {
          var cD = getChar(row + 1, col);
          if (isAlpha(cD)) {
            if (getChar(row + 2, col) == '.') {
              addWarp('$c$cD', row + 2, col);
              continue;
            }
            if (getChar(row - 1, col) == '.') {
              addWarp('$c$cD', row - 1, col);
              continue;
            }
          }
          cD = getChar(row, col + 1);
          if (isAlpha(cD)) {
            if (getChar(row, col + 2) == '.') {
              addWarp('$c$cD', row, col + 2);
              continue;
            }
            if (getChar(row, col - 1) == '.') {
              addWarp('$c$cD', row, col - 1);
              continue;
            }
          }
        }
      }
    }
  }

  String? getChar(int row, int col) {
    if (row < 0 || row >= lines.length) return null;
    if (col < 0 || col >= lines[0].length) return null;
    return lines[row].substring(col, col + 1);
  }

  void addWarp(String s, int row, int col) {
    if (s == 'AA' || s == 'ZZ') return;
    if (warps.containsKey(s)) {
      warps[s]!.p2 = LinePos(col, row);
    } else {
      warps[s] = Warp();
      warps[s]!.p1 = LinePos(col, row);
    }
  }

  int findShortestPath() {
    Set<Node> visited = {};
    List<Node> queue = [Node(startPoint, 0)];
    bool ready = false;
    while (true) {
      final node = getNodeWithLowestCost(queue);
      final c = (getChar(node.pos.row, node.pos.col));
      if (c != '.') continue;
      if (visited.contains(node)) continue;
      if (node.pos == endPoint) return node.distFromStart;
      visited.add(node);
      if (!ready) {
        for (final pos in node.pos.getNeighbours()) {
          queue.add(Node(pos, node.distFromStart + 1));
        }
        final warpPos = getWarpPos(node.pos);
        if (warpPos != null) {
          queue.add(Node(warpPos, node.distFromStart + 1));
        }
      }
    }
  }

  int findShortestPath2() {
    Set<Node2> visited = {};
    List<Node2> queue = [Node2(startPoint, 0, 0)];
    bool ready = false;
    while (true) {
      if (queue.isEmpty) return -1;
      final node = getNodeWithLowestCost2(queue);
      final c = (getChar(node.pos.row, node.pos.col));
      if (c != '.') continue;
      if (visited.contains(node)) continue;
      if (node.pos == endPoint && node.level == 0) return node.distFromStart;
      visited.add(node);
      if (!ready) {
        for (final pos in node.pos.getNeighbours()) {
          queue.add(Node2(pos, node.distFromStart + 1, node.level));
        }
        final warpPos = getWarpPos2(node.pos, node.level);
        if (warpPos != null) {
          queue.add(Node2(warpPos.pos, node.distFromStart + 1, warpPos.level));
        }
      }
    }
  }

  Node getNodeWithLowestCost(List<Node> queue) {
    Node lowestNode = Node(LinePos(0, 0), 999999999999999999);
    int lowestNodeNo = -1;
    for (int i = 0; i < queue.length; i++) {
      Node node = queue[i];
      if (node.distFromStart < lowestNode.distFromStart) {
        lowestNodeNo = i;
        lowestNode = node;
      }
    }
    queue.removeAt(lowestNodeNo);
    return lowestNode;
  }

  Node2 getNodeWithLowestCost2(List<Node2> queue) {
    Node2 lowestNode = Node2(LinePos(0, 0), 999999999999999999, 0);
    int lowestNodeNo = -1;
    for (int i = 0; i < queue.length; i++) {
      Node2 node = queue[i];
      if (node.distFromStart < lowestNode.distFromStart) {
        lowestNodeNo = i;
        lowestNode = node;
      }
    }
    queue.removeAt(lowestNodeNo);
    return lowestNode;
  }

  LinePos? getWarpPos(LinePos pos) {
    for (final warp in warps.entries) {
      if (warp.value.p1 == pos) return warp.value.p2;
      if (warp.value.p2 == pos) return warp.value.p1;
    }
    return null;
  }

  LevelChangePos? getWarpPos2(LinePos pos, int level) {
    for (final warp in warps.entries) {
      if (warp.value.p1 == pos) {
        if (isOuterMost(pos)) {
          if (level == 0) {
            return null; // Outermost portals dont function at level 0
          } else {
            return LevelChangePos(warp.value.p2!, level - 1);
          }
        } else {
          // Inner portal. Works on all levels.
          return LevelChangePos(warp.value.p2!, level + 1);
        }
      }
      if (warp.value.p2 == pos) {
        if (isOuterMost(pos)) {
          if (level == 0) {
            return null; // Outermost portals dont function at level 0
          } else {
            return LevelChangePos(warp.value.p1!, level - 1);
          }
        } else {
          // Inner portal. Works on all levels.
          return LevelChangePos(warp.value.p1!, level + 1);
        }
      }
    }
    return null;
  }

  bool isOuterMost(LinePos pos) {
    if (pos.col <= 2) return true;
    if (pos.row <= 2) return true;
    if (pos.col >= lines[0].length - 3) return true;
    if (pos.row >= lines.length - 3) return true;
    return false;
  }
}

class LevelChangePos {
  LinePos pos;
  int level;
  LevelChangePos(this.pos, this.level);
}

class Node {
  LinePos pos;
  int distFromStart;
  Node(this.pos, this.distFromStart);

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is Node && (this.pos == other.pos);
  }

  int get hashCode => pos.hashCode;
}

class Node2 {
  LinePos pos;
  int distFromStart;
  int level;
  Node2(this.pos, this.distFromStart, this.level);

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is Node2 &&
        (this.pos == other.pos && this.level == other.level);
  }

  int get hashCode => pos.hashCode + level.hashCode;
}
