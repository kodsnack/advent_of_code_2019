Future<void> main(List<String> args) async {
  const String input = '359282-820401';

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(input);
  print(resultP2);
}

int calcResultP1(String input) {
  int no1 = int.parse(input.split('-')[0]);
  int no2 = int.parse(input.split('-')[1]);
  int result = countValidNumbers(no1, no2);
  return result;
}

int calcResultP2(String input) {
  int no1 = int.parse(input.split('-')[0]);
  int no2 = int.parse(input.split('-')[1]);
  assert(isValidP2(112233));
  assert(!isValidP2(123444));
  assert(isValidP2(111122));
  int result = countValidNumbers(no1, no2, part2: true);
  return result;
}

int countValidNumbers(int no1, int no2, {bool part2 = false}) {
  int count = 0;
  for (int no = no1; no <= no2; no++) {
    if (part2) {
      if (isValidP2(no)) count++;
    } else {
      if (isValid(no)) count++;
    }
  }
  return count;
}

bool isValid(int no) {
  List<int> digits = no.toString().split('').map((e) => int.parse(e)).toList();

  if (digits.length != 6) return false;
  bool hasDigitPair = false;
  bool isDecreasing = false;
  for (int i = 1; i < digits.length; i++) {
    if (digits[i] == digits[i - 1]) hasDigitPair = true;
    if (digits[i] < digits[i - 1]) isDecreasing = true;
  }
  if (!hasDigitPair) return false;
  if (isDecreasing) return false;
  return true;
}

bool isValidP2(int no) {
  List<int> digits = no.toString().split('').map((e) => int.parse(e)).toList();

  if (digits.length != 6) return false;
  bool hasDigitPair = false;
  bool isDecreasing = false;
  int lastDigit = digits[0];
  int sameDigitCount = 1;
  for (int i = 1; i < digits.length; i++) {
    int nextDigit = digits[i];
    if (nextDigit == lastDigit) {
      sameDigitCount++;
    } else {
      if (sameDigitCount == 2) hasDigitPair = true;
      sameDigitCount = 1;
    }
    if (nextDigit < lastDigit) {
      isDecreasing = true;
      break;
    }
    lastDigit = nextDigit;
  }
  if (sameDigitCount == 2) hasDigitPair = true; // Last 2 digit was a pair.
  if (!hasDigitPair) return false;
  if (isDecreasing) return false;
  return true;
}
