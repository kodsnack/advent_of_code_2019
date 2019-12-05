#include <bits/stdc++.h>
#include "prettyprint.hpp"
using namespace std;

const int MAX_NUM_PARAMETERS = 3;

set<int> known_opcodes = {1, 2, 3, 4, 99};
map<int, int> num_parameters_for_opcode = {
    {1, 3},
    {2, 3},
    {3, 1},
    {4, 1},
    {99, 0}
};
vector<int> memory;
int memory_size;
int memory_get (int address) {
    if (0 <= address && address < memory_size)
        return memory[address];
    else {
        cerr << "error: out of bounds with address " << address << endl;
        return -1;
    }
}
void memory_set (int address, int value) {
    if (0 <= address && address < memory_size)
        memory[address] = value;
    else
        cerr << "error: out of bounds with address " << address << endl;
}

pair<int, vector<int>> extract_opcode_parameter_modes (int address) {
    int combined = memory_get(address);
    int opcode = combined % 100;
    combined /= 100;
    vector<int> parameter_modes (3);
    for (int i=0; i < MAX_NUM_PARAMETERS; i++) {
        parameter_modes[i] = combined % 10;
        combined /= 10;
    }
    return {opcode, parameter_modes};
}

const int PARAMETER_MODE_POSITION = 0;
const int PARAMETER_MODE_IMMEDIATE = 1;

vector<int> get_parameters (int address, int num_parameters, vector<int> parameter_modes) {
    vector<int> parameter_addresses (num_parameters);
    for (int i=0; i < num_parameters; i++) {
        switch (parameter_modes[i]) {
            case PARAMETER_MODE_POSITION:
            {
                parameter_addresses[i] = memory_get(address+i);
                break;
            }
            case PARAMETER_MODE_IMMEDIATE:
            {
                parameter_addresses[i] = address+i;
                break;
            }
        }
    }
    return parameter_addresses;
}

const int OPCODE_ADD        = 1;
const int OPCODE_MULTIPLY   = 2;
const int OPCODE_INPUT      = 3;
const int OPCODE_OUTPUT     = 4;
const int OPCODE_HALT       = 99;

int execute_operation(int opcode, vector<int> parameter_addresses) {
    switch (opcode) {
        case OPCODE_ADD:
        {
            int term1 = memory_get(parameter_addresses[0]);
            int term2 = memory_get(parameter_addresses[1]);
            int output_address = parameter_addresses[2];
            
            int output = term1 + term2;
            memory_set(output_address, output);
            break;
        }
        case OPCODE_MULTIPLY:
        {
            int factor1 = memory_get(parameter_addresses[0]);
            int factor2 = memory_get(parameter_addresses[1]);
            int output_address = parameter_addresses[2];

            int output = factor1 * factor2;
            memory_set(output_address, output);
            break;
        }
        case OPCODE_INPUT:
        {
            int output_address = parameter_addresses[0];
            int input;
            cin >> input;
            memory_set(output_address, input);
            break;
        }
        case OPCODE_OUTPUT:
        {
            int input_address = parameter_addresses[0];
            int output = memory_get(input_address);
            cout << output << endl;
            break;
        }
        case OPCODE_HALT:
        {
            return -1;
        }
    }
    return 0;
}

int main() {
    int code;
    char comma;
    while (cin >> code) {
        memory.push_back(code);
        if (cin.peek() == ',')
            cin >> comma;
        else
            break;
    }
    memory_size = memory.size();

    int address = 0;
    while (address < memory_size) {
        auto [opcode, parameter_modes] = extract_opcode_parameter_modes(address);
        vector<int> parameter_addresses = get_parameters(address+1, num_parameters_for_opcode[opcode], parameter_modes);
        if (execute_operation(opcode, parameter_addresses) == -1)
            break;
        address += 1+num_parameters_for_opcode[opcode];
    }
}

