import '../util/util.dart';

// const String inputFile = 'day16/example.txt';
const String inputFile = 'day16/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(input);
  print(resultP2);
}

String calcResultP1(String input) {
  List<int> signal = input.split('').map((e) => int.parse(e)).toList();

  // testFactor();

  for (var phaseNo = 0; phaseNo < 100; phaseNo++) {
    List<int> nextSignal = [];
    for (var digitIdx = 0; digitIdx < signal.length; digitIdx++) {
      int nextDigit = 0;
      for (int i = 0; i < signal.length; i++) {
        int factor = getFactor(i, digitIdx + 1);
        nextDigit += factor * signal[i];
      }
      nextDigit = nextDigit.abs() % 10;
      nextSignal.add(nextDigit.abs() % 10);
    }
    signal = nextSignal;
  }
  return signal.sublist(0, 8).join();
}

String calcResultP2(String input) {
  // Kind of cheated on this. Saw some pattern but got a lot of help from reddit.
  List<int> signalPart = input.split('').map((e) => int.parse(e)).toList();
  int messageOffset = int.parse(input.substring(0, 7));
  List<int> signal = [];
  for (int i = 0; i < 10000; i++) {
    signal.addAll(signalPart);
  }
  int totalLength = signal.length;
  signal.removeRange(0, messageOffset);
  signal = signal.reversed.toList();

  int remLength = signal.length;
  print('TL : $totalLength; RM : $remLength');
  print('Diff : ${totalLength - remLength}');

  for (var phaseNo = 0; phaseNo < 100; phaseNo++) {
    List<int> nextSignalBackwards = [];
    int sum = 0;
    for (int i = 0; i < signal.length; i++) {
      sum += signal[i];
      sum %= 10;
      nextSignalBackwards.add(sum);
    }
    signal = nextSignalBackwards;
  }

  final result = signal.reversed.take(8).join();
  return result;
}

int getFactor(int idx, int noOfRepeats) {
  List<int> baseRepeatingPattern = [0, 1, 0, -1];
  int patternLength = baseRepeatingPattern.length * noOfRepeats;
  int posInPattern = (idx + 1) % patternLength;
  int posInBasePattern = posInPattern ~/ noOfRepeats;
  int factor = baseRepeatingPattern[posInBasePattern];
  return factor;
}

void testFactor() {
  for (int noRepeats = 1; noRepeats <= 20; noRepeats++) {
    List<int> factors = [];
    for (int digitIdx = 0; digitIdx < 30; digitIdx++) {
      int factor = getFactor(digitIdx, noRepeats);
      factors.add(factor);
      //print('Digit : $digitIdx, Rep : $noRepeats, factor : $factor');
    }
    String s = '';
    for (int n in factors) {
      if (n >= 0) {
        s += ' $n, ';
      } else {
        s += '$n, ';
      }
    }
    print('( $s )');
  }
}
