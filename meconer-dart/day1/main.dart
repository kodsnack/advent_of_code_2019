import '../util/util.dart';

//const String inputFile = 'day1/example.txt';
const String inputFile = 'day1/input.txt';

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
  int sum = 0;
  print(calc(12));
  print(calc(14));
  print(calc(1969));
  print(calc(100756));
  for (final line in inputLines) {
    sum += calc(int.parse(line));
  }
  return sum;
}

int calcResultP2(List<String> inputLines) {
  int sum = 0;
  print(calcP2(14));
  print(calcP2(12));
  print(calcP2(1969));
  print(calcP2(100756));
  for (final line in inputLines) {
    sum += calcP2(int.parse(line));
  }
  return sum;
}

int calc(int number) {
  number = number ~/ 3 - 2;
  return number;
}

int calcP2(int number) {
  int fuel = calc(number);
  int total = fuel;
  while (fuel > 0) {
    fuel = calc(fuel);
    if (fuel > 0) total += fuel;
  }

  return total;
}
