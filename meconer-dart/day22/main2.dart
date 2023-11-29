import '../util/util.dart';

// const String inputFile = 'day22/example.txt';
const String inputFile = 'day22/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  test();

  print('Part 2:');
  final resultP2 = calcResultP2(input);
  print(resultP2);
}

num calcResultP1(String input) {
  Deck deck = Deck(10007);

  num cardToFollow = 2019;

  // newC = C*a + b
  // next time newNewC = newC*a +b = (C*a+b)*a + b; = C*a*a + a*b + b
  Params params = Params(1, 0);
  for (final line in input.split('\n')) {
    final tempParams = deck.handleCommandAB(line);
    num tempA = (params.a * tempParams.a) % deck.deckSize;
    params.b = (tempParams.a * params.b + tempParams.b) % deck.deckSize;
    params.a = tempA;
  }

  cardToFollow = (cardToFollow * params.a + params.b) % deck.deckSize;

  return cardToFollow;
}

num testP1Reversed(String input) {
  Deck deck = Deck(10007);

  // newC = C*a + b
  // next time newNewC = newC*a +b = (C*a+b)*a + b; = C*a*a + a*b + b
  Params params = Params(1, 0);
  for (final line in input.split('\n')) {
    final tempParams = deck.handleCommandInverseAB(line);
    num tempA = (params.a * tempParams.a) % deck.deckSize;
    params.b = (tempParams.a * params.b + tempParams.b) % deck.deckSize;
    params.a = tempA;
  }

  // Find the inverse of the function (a*x + b) mod n
  final aInv = modDiv(1, params.a, deck.deckSize);

  num result = ((5472 - params.b) * aInv) % deck.deckSize;

  return result;
}

num test() {
  String input = '''deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1''';
  Deck deck = Deck(10);

  List<num> startList = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
  Params params = Params(1, 0);
  for (final line in input.split('\n')) {
    final tempParams = deck.handleCommandAB(line);
    num tempA = (params.a * tempParams.a) % deck.deckSize;
    params.b = (tempParams.a * params.b + tempParams.b) % deck.deckSize;
    params.a = tempA;
  }

  List<num> answers = List.generate(10, (index) => index);
  for (int i = 0; i < startList.length; i++) {
    answers[(i * params.a.toInt() + params.b.toInt()) % deck.deckSize] =
        startList[i];
    ;
  }
  print(answers.join(', '));

  params = Params(1, 0);

  for (final line in input.split('\n').reversed) {
    final tempParams = deck.handleCommandInverseAB(line);
    num tempA = (params.a * tempParams.a) % deck.deckSize;
    params.b = (tempParams.a * params.b + tempParams.b) % deck.deckSize;
    params.a = tempA;
  }

  List<num> orgList = List.generate(10, (index) => index);
  for (int i = 0; i < answers.length; i++) {
    orgList[(i * params.a.toInt() + params.b.toInt()) % deck.deckSize] =
        answers[i];
    ;
  }
  print(orgList.join(', '));

  return 0;
}

// 22282207719044 too low (forgot to reverse input)
// 80965848102410 too high
// 99185713666050 too high
// 64586600795606 CORRECT!!! Finally. Had to use Bigint to prevent overflowing
num calcResultP2(String input) {
  Deck deck = Deck(119315717514047);

  // newC = C*a + b
  // next time newNewC = newC*a +b = (C*a+b)*a + b; = C*a*a + a*b + b
  Params params = Params(1, 0);
  for (final line in input.split('\n').reversed) {
    final tempParams = deck.handleCommandInverseAB(line);
    BigInt tA = BigInt.from(tempParams.a);
    BigInt tB = BigInt.from(tempParams.b);
    BigInt a = BigInt.from(params.a);
    BigInt b = BigInt.from(params.b);
    final tempA = (a * tA) % BigInt.from(deck.deckSize);
    b = (tA * b + tB) % BigInt.from(deck.deckSize);
    if (!tempA.isValidInt || !b.isValidInt) throw Error();
    params.a = tempA.toInt();
    params.b = b.toInt();
  }

  // Calculate the function many times
  num count = 101741582076661;
  final resFuncParams = modPow(params, count, deck.deckSize);

  num result = (2020 * resFuncParams.a + resFuncParams.b) % deck.deckSize;

  return result;
}

class Params {
  num a, b;
  Params(this.a, this.b);
}

// modSize has to be a prime
int modDiv(num a, num b, int modSize) {
  while ((a ~/ b) * b != a) {
    a += modSize;
  }
  return a ~/ b;
}

//  modpow the polynomial: (ax+b)^m % n
//  f(x) = ax+b
//  g(x) = cx+d
//  f^2(x) = a(ax+b)+b = aax + ab+b
//  f(g(x)) = a(cx+d)+b = acx + ad+b
// Recursively with either squaring (if m is even) or
Params modPow(Params params, num m, num n) {
  if (m == 0) return Params(1, 0); // Final case

  if (m % 2 == 0) {
    // m is even. We square the function
    BigInt ba = BigInt.from(params.a);
    BigInt bb = BigInt.from(params.b);
    BigInt na = ba * ba % BigInt.from(n);
    BigInt nb = (ba * bb + bb) % BigInt.from(n);
    if (!na.isValidInt || !nb.isValidInt) throw Error();
    Params newParams = Params(na.toInt(), nb.toInt());
    return modPow(newParams, m ~/ 2, n);
  }

  Params newParams = modPow(params, m - 1, n);
  BigInt ba = BigInt.from(params.a);
  BigInt bb = BigInt.from(params.b);
  BigInt c = BigInt.from(newParams.a);
  BigInt d = BigInt.from(newParams.b);
  final a = (ba * c) % BigInt.from(n);
  final b = (ba * d + bb) % BigInt.from(n);
  if (!a.isValidInt || !b.isValidInt) throw Error();
  return Params(a.toInt(), b.toInt());
}

class Deck {
  int deckSize;

  int cardToFollow = -1;

  Deck(this.deckSize);

  void dealIntoNew() {
    cardToFollow = deckSize - cardToFollow - 1;
  }

  Params dealIntoNewAB() {
    return Params(-1, deckSize - 1);
  }

  void dealIntoNewInverse() {
    // Just reverses so inverse is same
    dealIntoNew();
  }

  Params dealIntoNewInverseAB() {
    return Params(-1, deckSize - 1);
  }

  void cutN(int n) {
    if (n < 0) n = (deckSize + n) % deckSize;
    cardToFollow -= n;
    cardToFollow %= deckSize;
  }

  Params cutNAB(int n) {
    return Params(1, -n);
  }

  void cutNInverse(int n) {
    if (n < 0) n = (deckSize + n) % deckSize;
    cardToFollow += n;
    cardToFollow %= deckSize;
  }

  Params cutNInverseAB(int n) {
    return Params(1, n);
  }

  void dealIncrement(int n) {
    cardToFollow = cardToFollow * n % deckSize;
  }

  Params dealIncrementAB(int n) {
    return Params(n, 0);
  }

  void dealIncrementInverse(int n) {
    int i = cardToFollow;
    int divisor = n;
    while ((i ~/ divisor) * divisor != i) {
      i += deckSize;
    }
    cardToFollow = i ~/ divisor;
  }

  Params dealIncrementInverseAB(int n) {
    int aInv = n.modInverse(deckSize); // modDiv(1, n, deckSize);
    return Params(aInv, 0);
  }

  void handleCommand(String line) {
    if (line.contains('deal into new stack')) {
      dealIntoNew();
    }
    if (line.contains('deal with increment')) {
      dealIncrement(int.parse(line.split(' ').last));
    }
    if (line.contains('cut')) {
      cutN(int.parse(line.split(' ').last));
    }
  }

  Params handleCommandAB(String line) {
    if (line.contains('deal into new stack')) {
      return dealIntoNewAB();
    }
    if (line.contains('deal with increment')) {
      return dealIncrementAB(int.parse(line.split(' ').last));
    }
    if (line.contains('cut')) {
      return cutNAB(int.parse(line.split(' ').last));
    }
    throw Exception('Illegal line');
  }

  void handleCommandInverse(String line) {
    if (line.contains('deal into new stack')) {
      dealIntoNewInverse();
    }
    if (line.contains('deal with increment')) {
      dealIncrementInverse(int.parse(line.split(' ').last));
    }
    if (line.contains('cut')) {
      cutNInverse(int.parse(line.split(' ').last));
    }
  }

  Params handleCommandInverseAB(String line) {
    if (line.contains('deal into new stack')) {
      return dealIntoNewInverseAB();
    }
    if (line.contains('deal with increment')) {
      return dealIncrementInverseAB(int.parse(line.split(' ').last));
    }
    if (line.contains('cut')) {
      return cutNInverseAB(int.parse(line.split(' ').last));
    }
    throw Exception('Illegal line');
  }

  void followCard(int cardNo) {
    cardToFollow = cardNo;
  }

  int getIndexOfFollowedCard() {
    return cardToFollow;
  }
}
