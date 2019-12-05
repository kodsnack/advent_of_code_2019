#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#define STEP 4

bool is_valid_opcode(int o) {
    if(o == 1 || o == 2 || o == 99) {
        return true;
    }
    return false;
}

int main() {
    // puzzle input
    int code[] = {1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,10,23
                    ,27,2,27,13,31,1,10,31,35,1,35,9,39,2,39,13,43,1,43,5,47
                    ,1,47,6,51,2,6,51,55,1,5,55,59,2,9,59,63,2,6,63,67,1,13
                    ,67,71,1,9,71,75,2,13,75,79,1,79,10,83,2,83,9,87,1,5,87
                    ,91,2,91,6,95,2,13,95,99,1,99,5,103,1,103,2,107,1,107
                    ,10,0,99,2,0,14,0};
    // change input as instructed
    code[1] = 12;
    code[2] = 2;

    int size = sizeof(code) / sizeof(code[0]);

    for(int i = 0; i < size; i += STEP) {
        int op = code[i];
        int addr_a = code[i+1];
        int addr_b = code[i+2];
        int addr_c = code[i+3];

        if(is_valid_opcode(op)) {
            if(op == 99) { // exit
                printf("value at postion 0: %d\n", code[0]);
                exit(EXIT_SUCCESS);
            } else if(op == 1) { // add
                code[addr_c] = code[addr_a] + code[addr_b];
            } else if(op == 2) { // multiply
                code[addr_c] = code[addr_a] * code[addr_b];
            }
        } else {
            printf("Error: %d is not a valid opcode\n", op);
            exit(EXIT_FAILURE);
        }
    }
}
