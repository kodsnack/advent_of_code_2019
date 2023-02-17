import '../util/util.dart';

// const String inputFile = 'day12/example.txt';
const String inputFile = 'day12/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInput(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(input);
  print(resultP2);
}

int calcResultP1(List<String> input) {
  List<Moon> moons = [];
  for (final line in input) {
    int x = int.parse(line.split(',')[0].split('=')[1]);
    int y = int.parse(line.split(',')[1].split('=')[1]);
    int z = int.parse(line.split(',')[2].split('=')[1].replaceAll('>', ''));
    moons.add(Moon([x, y, z]));
  }
  for (int i = 0; i < 1000; i++) {
    // Apply gravity
    for (int firstMoonNo = 0; firstMoonNo < moons.length - 1; firstMoonNo++) {
      for (int secondMoonNo = firstMoonNo + 1;
          secondMoonNo < moons.length;
          secondMoonNo++) {
        moons[firstMoonNo].applyGravity(moons[secondMoonNo]);
        moons[secondMoonNo].applyGravity(moons[firstMoonNo]);
      }
    }
    // Apply velocity
    for (int moonNo = 0; moonNo < moons.length; moonNo++) {
      moons[moonNo].applyVelocity();
    }
  }
  int energy = 0;
  for (int moonNo = 0; moonNo < moons.length; moonNo++) {
    energy += moons[moonNo].potEnergy() * moons[moonNo].kinEnergy();
  }
  return energy;
}

int calcResultP2(List<String> input) {
  List<Moon> moons = [];
  for (final line in input) {
    int x = int.parse(line.split(',')[0].split('=')[1]);
    int y = int.parse(line.split(',')[1].split('=')[1]);
    int z = int.parse(line.split(',')[2].split('=')[1].replaceAll('>', ''));
    moons.add(Moon([x, y, z]));
  }

  // First find when the cycles for x and y repeats.
  List<bool> foundCycleLengths = [];
  List<Set<String>> states = [{}, {}, {}];
  for (int axisNo = 0; axisNo < 3; axisNo++) {
    states[axisNo].add(getState(moons, axisNo));
    foundCycleLengths.add(false);
  }

  while (foundCycleLengths.any((element) => !element)) {
    // Apply gravity
    for (int firstMoonNo = 0; firstMoonNo < moons.length - 1; firstMoonNo++) {
      for (int secondMoonNo = firstMoonNo + 1;
          secondMoonNo < moons.length;
          secondMoonNo++) {
        moons[firstMoonNo].applyGravity(moons[secondMoonNo]);
        moons[secondMoonNo].applyGravity(moons[firstMoonNo]);
      }
    }
    // Apply velocity
    for (int moonNo = 0; moonNo < moons.length; moonNo++) {
      moons[moonNo].applyVelocity();
      //moons[moonNo].printMoon();
    }

    // Check states
    for (int axisNo = 0; axisNo < 3; axisNo++) {
      String state = getState(moons, axisNo);
      if (states[axisNo].contains(state)) {
        foundCycleLengths[axisNo] = true;
      } else {
        states[axisNo].add(state);
      }
    }
  }

  List<int> cycleLengths = List.generate(3, (index) => states[index].length);
  cycleLengths.sort(((a, b) => b.compareTo(a)));
  int increment = cycleLengths.first;
  int cycles = increment;
  int nextAxis = 1;

  while (nextAxis < 3) {
    int remainder = cycles % cycleLengths[nextAxis];
    if (remainder != 0) {
      cycles += increment;
    } else {
      increment = cycles;
      nextAxis++;
    }
  }

  return cycles;
}

String getState(List<Moon> moons, int axisNo) {
  String state = '';
  for (var moonNo = 0; moonNo < moons.length; moonNo++) {
    int pos = moons[moonNo].pos[axisNo];
    int vel = moons[moonNo].vel[axisNo];
    state += '$pos,$vel,';
  }
  return state;
}

class Moon {
  List<int> pos;
  List<int> vel = [0, 0, 0];

  Moon(this.pos);

  void applyGravity(Moon otherMoon) {
    for (int i = 0; i < 3; i++) {
      vel[i] += (otherMoon.pos[i] - pos[i]).sign;
    }
  }

  void applyVelocity() {
    for (int i = 0; i < 3; i++) {
      pos[i] += vel[i];
    }
  }

  void printMoon() {
    print(
        'pos=<x=${pos[0]}, y=${pos[1]}, z=${pos[2]}>, vel=<x=${vel[0]}, y=${vel[1]}, z=${vel[2]}>');
  }

  int potEnergy() {
    int energy = pos.fold(0, (a, b) => a + b.abs());
    return energy;
  }

  int kinEnergy() {
    return vel.fold(0, (a, b) => a + b.abs());
  }
}
