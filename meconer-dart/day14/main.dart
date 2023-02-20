import '../util/util.dart';

// const String inputFile = 'day14/example.txt';
const String inputFile = 'day14/input.txt';

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
  Map<String, Reaction> reactions = {};
  for (final line in input) {
    String name = line.split('=>')[1].trim().split(' ')[1];
    int amount = int.parse(line.split('=>')[1].trim().split(' ')[0]);
    Reaction reaction = Reaction(name, amount, []);
    final ingredientsPart = line.split('=>')[0].trim().split(',');
    for (final ingredientPart in ingredientsPart) {
      String ingrName = ingredientPart.trim().split(' ')[1];
      int ingrAmount = int.parse(ingredientPart.trim().split(' ')[0]);
      Chemical chemical = Chemical(ingrName, ingrAmount);
      reaction.ingredients.add(chemical);
    }
    reactions[name] = reaction;
  }

  Map<String, int> needed = {'FUEL': 1};
  Map<String, int> store = {};

  while (!isOnlyOre(needed)) {
    Map<String, int> nextNeed = {};
    for (final need in needed.entries) {
      String ingrNeeded = need.key;
      int amountNeeded = need.value;

      if (ingrNeeded == 'ORE') {
        addNeed(nextNeed, 'ORE', amountNeeded);
        continue;
      }

      final recepy = reactions[ingrNeeded]!;
      if (store.containsKey(ingrNeeded)) {
        int amountInStore = store[ingrNeeded]!;

        if (amountInStore > amountNeeded) {
          store[ingrNeeded] = amountInStore - amountNeeded;
          continue;
        }

        amountNeeded -= amountInStore;
        store[ingrNeeded] = 0;
      }

      if (amountNeeded == 0) continue;
      int amountMade = amountNeeded;
      if (recepy.amount > amountNeeded) amountMade = recepy.amount;

      int reactionCount = 1;
      if (recepy.amount < amountNeeded) {
        reactionCount = amountNeeded ~/ recepy.amount;
        if (amountNeeded % recepy.amount > 0) reactionCount++;
        amountMade = recepy.amount * reactionCount;
      }
      if (amountMade > amountNeeded) {
        addToStore(store, ingrNeeded, amountMade - amountNeeded);
      }

      for (final ingredient in recepy.ingredients) {
        addNeed(nextNeed, ingredient.name, ingredient.amount * reactionCount);
      }
    }
    needed = nextNeed;
  }
  return needed['ORE']!;
}

int calcResultP2(List<String> input) {
  Map<String, Reaction> reactions = {};
  for (final line in input) {
    String name = line.split('=>')[1].trim().split(' ')[1];
    int amount = int.parse(line.split('=>')[1].trim().split(' ')[0]);
    Reaction reaction = Reaction(name, amount, []);
    final ingredientsPart = line.split('=>')[0].trim().split(',');
    for (final ingredientPart in ingredientsPart) {
      String ingrName = ingredientPart.trim().split(' ')[1];
      int ingrAmount = int.parse(ingredientPart.trim().split(' ')[0]);
      Chemical chemical = Chemical(ingrName, ingrAmount);
      reaction.ingredients.add(chemical);
    }
    reactions[name] = reaction;
  }

  // For startVal I just tested some values
  int startVal = 3700000;
  int limit = 1000000000000;
  int result = 0;
  while (result <= limit) {
    Map<String, int> needed = {'FUEL': startVal};
    Map<String, int> store = {};

    while (!isOnlyOre(needed)) {
      Map<String, int> nextNeed = {};
      for (final need in needed.entries) {
        String ingrNeeded = need.key;
        int amountNeeded = need.value;

        if (ingrNeeded == 'ORE') {
          addNeed(nextNeed, 'ORE', amountNeeded);
          continue;
        }

        final recepy = reactions[ingrNeeded]!;
        if (store.containsKey(ingrNeeded)) {
          int amountInStore = store[ingrNeeded]!;

          if (amountInStore > amountNeeded) {
            store[ingrNeeded] = amountInStore - amountNeeded;
            continue;
          }

          amountNeeded -= amountInStore;
          store[ingrNeeded] = 0;
        }

        if (amountNeeded == 0) continue;
        int amountMade = amountNeeded;
        if (recepy.amount > amountNeeded) amountMade = recepy.amount;

        int reactionCount = 1;
        if (recepy.amount < amountNeeded) {
          reactionCount = amountNeeded ~/ recepy.amount;
          if (amountNeeded % recepy.amount > 0) reactionCount++;
          amountMade = recepy.amount * reactionCount;
        }
        if (amountMade > amountNeeded) {
          addToStore(store, ingrNeeded, amountMade - amountNeeded);
        }

        for (final ingredient in recepy.ingredients) {
          addNeed(nextNeed, ingredient.name, ingredient.amount * reactionCount);
        }
      }
      needed = nextNeed;
    }
    result = needed['ORE']!;
    if (result > limit) {
      startVal--;
    } else {
      startVal += 1;
    }
  }
  return startVal;
}

void addToStore(Map<String, int> store, String ingrNeeded, int amountToStore) {
  if (store.containsKey(ingrNeeded)) {
    amountToStore += store[ingrNeeded]!;
  }
  store[ingrNeeded] = amountToStore;
}

void addNeed(Map<String, int> nextNeed, String key, int amountNeeded) {
  if (nextNeed.containsKey(key)) {
    amountNeeded += nextNeed[key]!;
  }
  nextNeed[key] = amountNeeded;
}

bool isOnlyOre(Map<String, int> needed) {
  for (final key in needed.keys) {
    if (key != 'ORE' && needed[key]! > 0) return false;
  }
  return true;
}

int reduceToOre(String name, int amount, Map<String, Reaction> reactions,
    Map<String, int> store) {
  if (name == 'ORE') return amount;
  int oreCount = 0;
  for (final ingredient in reactions[name]!.ingredients) {
    oreCount +=
        reduceToOre(ingredient.name, ingredient.amount, reactions, store);
  }
  return oreCount;
}

class Reaction {
  String result;
  int amount;
  List<Chemical> ingredients;
  Reaction(this.result, this.amount, this.ingredients);
}

class Chemical {
  int amount;
  String name;
  Chemical(this.name, this.amount);
}
