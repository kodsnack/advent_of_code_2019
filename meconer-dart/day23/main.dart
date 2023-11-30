import '../util/intcodecomputer.dart';
import '../util/util.dart';

const String inputFile = 'day23/input.txt';

Future<void> main(List<String> args) async {
  var program = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(program);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(program);
  print(resultP2);
}

int calcResultP1(String program) {
  int computerCount = 50;

  List<IntCodeComputer> computers = [];
  // Build and boot up all the computers
  for (int compNo = 0; compNo < computerCount; compNo++) {
    IntCodeComputer computer = IntCodeComputer(program);
    computers.add(computer);
    computer.inputQueue.add(compNo);
    computer.runProgram();
  }

  int computerNo = 0;
  List<Message> messages = [];
  while (true) {
    final message = getMessageFor(computerNo, messages);
    if (message != null) {
      computers[computerNo].inputQueue.add(message.x);
      computers[computerNo].inputQueue.add(message.y);
    } else {
      computers[computerNo].inputQueue.add(-1);
    }
    computers[computerNo].runProgram();
    while (computers[computerNo].outputQueue.isNotEmpty) {
      int recipient = computers[computerNo].outputQueue.removeAt(0);
      int x = computers[computerNo].outputQueue.removeAt(0);
      int y = computers[computerNo].outputQueue.removeAt(0);
      final message = Message(recipient, x, y);
      messages.add(message);
      if (recipient == 255) {
        return message.y;
      }
    }
    computerNo++;
    computerNo %= computerCount;
  }
}

int calcResultP2(String program) {
  int computerCount = 50;

  List<IntCodeComputer> computers = [];
  // Build and boot up all the computers
  for (int compNo = 0; compNo < computerCount; compNo++) {
    IntCodeComputer computer = IntCodeComputer(program);
    computers.add(computer);
    computer.inputQueue.add(compNo);
    computer.runProgram();
  }

  int computerNo = 0;
  List<Message> messages = [];
  Message lastNatMessage = Message(255, -1, -1);
  int lastYValueSent = -1;

  while (true) {
    // Check if this computer has a message and add that to its input queue
    final message = getMessageFor(computerNo, messages);
    if (message != null) {
      computers[computerNo].inputQueue.add(message.x);
      computers[computerNo].inputQueue.add(message.y);
    } else {
      computers[computerNo].inputQueue.add(-1);
    }

    // Run this computer until it stops
    computers[computerNo].runProgram();

    // Check if it has output and add those to the messages list
    while (computers[computerNo].outputQueue.isNotEmpty) {
      int recipient = computers[computerNo].outputQueue.removeAt(0);
      int x = computers[computerNo].outputQueue.removeAt(0);
      int y = computers[computerNo].outputQueue.removeAt(0);
      final message = Message(recipient, x, y);
      messages.add(message);
    }

    // See if we have a message for the NAT (Not Always Transmitting...)
    final messageForNat = getMessageFor(255, messages);
    if (messageForNat != null) {
      lastNatMessage =
          Message(messageForNat.recipient, messageForNat.x, messageForNat.y);
    }

    // If there are no more messages in the list. Check to see if the input queues
    // are empty too. In that case the NAT tries to restart the system by
    // sending the last NAT message to computer 0.
    if (messages.isEmpty) {
      int noOfMessagesInPacketQueues = countMessagesInPacketQueues(computers);
      if (noOfMessagesInPacketQueues == 0) {
        // Add last NAT message to input queue of computer 0;
        computers[0].addInput(lastNatMessage.x);
        computers[0].addInput(lastNatMessage.y);

        // If we have sent this same message twice in a row we are ready so
        // just return the value
        if (lastNatMessage.y == lastYValueSent && lastYValueSent != -1)
          return lastYValueSent;
        lastYValueSent = lastNatMessage.y;
      }
    }
    computerNo++;
    computerNo %= computerCount;
  }
}

int countMessagesInPacketQueues(List<IntCodeComputer> computers) {
  int count = 0;
  for (final computer in computers) {
    count += computer.inputQueue.length;
  }
  return count;
}

Message? getMessageFor(int computerNo, List<Message> messages) {
  for (int i = 0; i < messages.length; i++) {
    if (messages[i].recipient == computerNo) {
      final message = messages.removeAt(i);
      return message;
    }
  }
  return null;
}

class Message {
  int recipient;
  int x;
  int y;
  Message(this.recipient, this.x, this.y);
}
