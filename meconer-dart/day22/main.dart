import '../util/util.dart';

// const String inputFile = 'day22/example.txt';
const String inputFile = 'day22/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  // print('Part 2:');
  // final resultP2 = calcResultP2(input);
  // print(resultP2);
}

int calcResultP1(String input) {
  Deck deck = Deck(10007);
  for (final line in input.split('\n')) {
    deck = deck.handleCommand(line);
  }
  return deck.cards.indexWhere((element) => element == 2019);
}

class Deck {
  late List<int> cards;

  Deck(int size) {
    cards = List.generate(size, (index) => index);
  }

  Deck.fromCards(List<int> cards) {
    this.cards = cards;
  }

  Deck.empty() {}

  Deck dealIntoNew() {
    final newDeck = Deck.empty();
    newDeck.cards = [...cards.reversed];
    return newDeck;
  }

  Deck cutN(int n) {
    final newDeck = Deck.empty();
    if (n > 0) {
      newDeck.cards = [
        ...cards.sublist(n, cards.length),
        ...cards.sublist(0, n)
      ];
    } else {
      newDeck.cards = [
        ...cards.sublist(cards.length + n),
        ...cards.sublist(0, cards.length + n)
      ];
    }
    return newDeck;
  }

  Deck dealIncrement(int n) {
    int insertPos = 0;
    final newDeck = Deck(cards.length);
    for (int i = 0; i < cards.length; i++) {
      newDeck.cards[insertPos] = cards[i];
      insertPos = (insertPos + n) % cards.length;
    }
    return newDeck;
  }

  Deck handleCommand(String line) {
    if (line == 'deal into new stack') {
      return dealIntoNew();
    }
    if (line.contains('deal with increment')) {
      return dealIncrement(int.parse(line.split(' ').last));
    }
    if (line.contains('cut')) {
      return cutN(int.parse(line.split(' ').last));
    }
    return Deck.empty();
  }
}
