import 'dart:math';

import '../util/util.dart';

//const String inputFile = 'day6/example.txt';
const String inputFile = 'day6/input.txt';

Future<void> main(List<String> args) async {
  var inputLines = await readInput(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(inputLines);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(inputLines);
  print(resultP2);
}

int calcResultP1(List<String> inputLines) {
  final orbits = buildOrbits(inputLines);
  int orbitCount = countOrbits(orbits);
  return orbitCount;
}

int calcResultP2(List<String> inputLines) {
  final orbits = buildOrbits(inputLines);
  int youLevel = getLevel(orbits, 'YOU', 'COM');
  int sanLevel = getLevel(orbits, 'SAN', 'COM');
  int deepestCommonLevel = getDeepestCommonLevel(orbits);

  int result = youLevel + sanLevel - 2 - 2 * deepestCommonLevel;
  return result;
}

int getDeepestCommonLevel(Map<String, List<String>> orbits) {
  int deepestLevel = 0;
  for (final orbiter in orbits['COM']!) {
    int level = recDeepestLevel(orbits, orbiter, 1);
    deepestLevel = max(deepestLevel, level);
  }
  return deepestLevel;
}

int recDeepestLevel(
    Map<String, List<String>> orbits, String orbiter, int level) {
  if (!orbits.containsKey(orbiter)) return -1;
  bool containsYou = doesContain(orbits, 'YOU', orbiter);
  bool containsSan = doesContain(orbits, 'SAN', orbiter);
  if (!containsSan || !containsYou) return -1;

  for (final subOrbiter in orbits[orbiter]!) {
    int subLevel = recDeepestLevel(orbits, subOrbiter, level + 1);
    level = max(level, subLevel);
  }
  return level;
}

int getLevel(
    Map<String, List<String>> orbits, String orbitterToFind, String start) {
  int level = 0;
  for (final orbiter in orbits[start]!) {
    level = recFind(orbits, orbiter, orbitterToFind, level + 1);
  }
  return level;
}

bool doesContain(
    Map<String, List<String>> orbits, String orbitterToFind, String start) {
  int level = 0;
  for (final orbiter in orbits[start]!) {
    level = recFind(orbits, orbiter, orbitterToFind, level + 1);
    if (level > 0) return true;
  }
  return false;
}

int recFind(Map<String, List<String>> orbits, String orbiter,
    String orbitterToFind, int level) {
  if (orbiter == orbitterToFind) return level;
  if (!orbits.containsKey(orbiter)) return -1;
  for (final subOrbiter in orbits[orbiter]!) {
    int subLevel = recFind(orbits, subOrbiter, orbitterToFind, level + 1);
    if (subLevel > 0) return subLevel;
  }
  return -1;
}

int countOrbits(Map<String, List<String>> orbits) {
  int orbitCount = 0;
  for (final orbiter in orbits['COM']!) {
    orbitCount += recOrbitCount(orbiter, orbits, 1);
  }
  return orbitCount;
}

int recOrbitCount(String orbiter, Map<String, List<String>> orbits, int level) {
  if (!orbits.containsKey(orbiter)) return level;
  int orbitCount = level;
  for (final subOrbiter in orbits[orbiter]!) {
    orbitCount += recOrbitCount(subOrbiter, orbits, level + 1);
    print(' $orbiter / $subOrbiter : $orbitCount');
  }
  return orbitCount;
}

Map<String, List<String>> buildOrbits(List<String> inputLines) {
  Map<String, List<String>> orbits = {};
  for (final line in inputLines) {
    String center = line.split(')')[0];
    String orbiter = line.split(')')[1];
    if (orbits.containsKey(center)) {
      orbits[center]!.add(orbiter);
    } else {
      orbits[center] = [orbiter];
    }
  }
  return orbits;
}
