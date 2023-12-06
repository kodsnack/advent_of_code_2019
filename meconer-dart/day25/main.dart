import 'dart:math';

import '../util/intcodecomputer.dart';
import '../util/util.dart';

const String inputFile = 'day25/input.txt';

Future<void> main(List<String> args) async {
  var program = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(program);
  print(resultP1);

  // print('Part 2:');
  // final resultP2 = calcResultP2(program);
  // print(resultP2);
}

List<String> commands = [
  'west',
  'take fixed point',
  'north',
  'take sand',
  'south',
  'east',
  'east',
  'take asterisk',
  'east',
  'west',
  'north',
  'north',
  'take hypercube',
  'north',
  'take coin',
  'north',
  'take easter egg',
  'west',
  'east',
  'south',
  'south',
  'west',
  'east',
  'south',
  'west',
  'north',
  'take spool of cat6',
  'north',
  'take shell',
  'west',
  'north',
  'inv',
  'north',
];

const inventoryItems = [
  'fixed point',
  'sand',
  'easter egg',
  'coin',
  'spool of cat6',
  'hypercube',
  'asterisk',
  'shell',
];

String? calcResultP1(String program) {
  IntCodeComputer computer = IntCodeComputer(program);
  computer.runProgram();
  final output = computer.outputQueue;
  print(output.map((e) => String.fromCharCode(e)).join());
  for (final command in commands) {
    print(command);
    computer.addTextInput(command);
    computer.runProgram();
    print(output.map((e) => String.fromCharCode(e)).join());
    print('---');
  }
  for (final invItem in inventoryItems) {
    computer.addTextInput('drop $invItem');
    computer.runProgram();
    print(output.map((e) => String.fromCharCode(e)).join());
  }

  for (int bitmaskForItemsToCarry = 1;
      bitmaskForItemsToCarry < pow(2, inventoryItems.length);
      bitmaskForItemsToCarry++) {
    List<String> invItemsToTake = [];
    for (int idx = 0; idx < inventoryItems.length; idx++) {
      int currMask = 1 << idx;
      if (bitmaskForItemsToCarry & currMask != 0)
        invItemsToTake.add(inventoryItems[idx]);
    }
    for (final invItem in invItemsToTake) {
      computer.addTextInput('take $invItem');
      computer.runProgram();
      String outputString = output.map((e) => String.fromCharCode(e)).join();
      print(outputString);
    }
    computer.addTextInput('north');
    computer.runProgram();
    String outputString = output.map((e) => String.fromCharCode(e)).join();
    print(outputString);

    if (outputString.contains('heavier')) {
      print('*** Too light');
    } else if (outputString.contains('lighter')) {
      print('*** Too heavy');
    } else {
      print('Ready!');
      final result = RegExp(r'\d+').firstMatch(outputString)!.group(0);
      return result;
    }

    for (final invItem in invItemsToTake) {
      computer.addTextInput('drop $invItem');
      computer.runProgram();
      outputString = output.map((e) => String.fromCharCode(e)).join();
      print(outputString);
    }
  }
  return null;
}
