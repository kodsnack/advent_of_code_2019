#include <iostream>

#include "intcode.h"

void p02(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    const auto input = readIntcode(is);

    for(int noun = 0; noun < 100; noun++) {
        for(int verb = 0; verb < 100; verb++) {
            intcodemachine mach(input);
            mach.data[1] = noun;
            mach.data[2] = verb;

            while(mach.step());

            if(noun == 12 && verb == 2) {
                ans1 = mach.data[0];
            }
            if(mach.data[0] == 19690720) {
                ans2 = 100*noun + verb;
            }
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p02(std::cin);
}
