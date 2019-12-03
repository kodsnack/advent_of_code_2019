#include <bits/stdc++.h>
using namespace std;

int main() {
    vector<int> program;
    int code;
    char comma;
    while (cin >> code) {
        cin >> comma;
        program.push_back(code);
    }
    int lookingfor = 19690720;

    for (int noun=0; noun <= 99; noun++) {
        for (int verb=0; verb <= 99; verb++) {
            vector<int> memory = program;
            memory[1] = noun;
            memory[2] = verb;
            int final_output = 0;
            for (int pos=0; pos < memory.size(); pos += 4) {
                int opcode = memory[pos];
                if (opcode == 99) {
                    final_output = memory[0];
                    break;
                }
                if (opcode != 1 && opcode != 2) {
                    cout << "error: unknown opcode (" << opcode << ") at position " << pos << endl;
                    break;
                }
                int num1 = memory[pos+1];
                if (num1 < 0 || num1 >= memory.size()) {
                    cout << "error: num1 at position " << pos+1 << " is out of bounds" << endl;
                    break;
                }
                int num2 = memory[pos+2];
                if (num2 < 0 || num2 >= memory.size()) {
                    cout << "error: num2 at position " << pos+2 << " is out of bounds" << endl;
                    break;
                }
                int output = memory[pos+3];
                if (output < 0 || output >= memory.size()) {
                    cout << "error: output position at position " << pos+3 << " is out of bounds" << endl;
                    break;
                }
                if (opcode == 1)
                    memory[output] = memory[num1] + memory[num2];
                if (opcode == 2)
                    memory[output] = memory[num1] * memory[num2];
            }
            if (final_output == lookingfor) {
                cout << 100 * noun + verb << endl;
            }
        }
    }
}

