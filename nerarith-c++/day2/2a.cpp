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

    program[1] = 12;
    program[2] = 2;
    for (int pos=0; pos < program.size(); pos += 4) {
        int opcode = program[pos];
        if (opcode == 99) {
            cout << program[0] << endl;
            return 0;
        }
        if (opcode != 1 && opcode != 2) {
            cout << "error: unknown opcode" << endl;
            return 0;
        }
        int num1 = program[pos+1];
        if (num1 < 0 || num1 >= program.size()) {
            cout << "error: num1 at position " << pos+1 << " is out of bounds" << endl;
            return 0;
        }
        int num2 = program[pos+2];
        if (num2 < 0 || num2 >= program.size()) {
            cout << "error: num2 at position " << pos+2 << " is out of bounds" << endl;
            return 0;
        }
        int output = program[pos+3];
        if (output < 0 || output >= program.size()) {
            cout << "error: output position at position " << pos+3 << " is out of bounds" << endl;
            return 0;
        }
        if (opcode == 1)
            program[output] = program[num1] + program[num2];
        if (opcode == 2)
            program[output] = program[num1] * program[num2];
    }
}

