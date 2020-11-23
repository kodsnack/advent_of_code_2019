#include <iostream>
#include <vector>
#include <tuple>
#include <map>

#include "intcode.h"

void p21(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    const auto program = readIntcode(is);

    std::string inputstring1 =
            "NOT A J\n"
            "NOT B T\n"
            "OR T J\n"
            "NOT C T\n"
            "OR T J\n"
            "AND D J\n"
            "WALK\n";

    std::string inputstring2 =
            "NOT A J\n"
            "NOT B T\n"
            "OR T J\n"
            "NOT C T\n"
            "OR T J\n"
            "AND D J\n"

            "NOT E T\n"
            "NOT T T\n"
            "OR H T\n"
            "AND T J\n"
            "RUN\n";

    for(auto problem : {1,2}) {
        intcodemachine mach(program);
        for (auto c : problem == 1 ? inputstring1 : inputstring2) {
            mach.addInput(c);
        }

        bool done = false;
        while (!done) {
            while (mach.step());
            switch (mach.state) {
                case intcodemachine::RUNNING:
                    // do nothing, we should not end up here
                    break;
                case intcodemachine::WAITING:
                    std::cout << "Input missing!" << std::endl;
                    done = true;
                    break;
                case intcodemachine::OUTPUT:
                    if (mach.output > 128) {
                        (problem == 1 ? ans1 : ans2) = mach.output;
                    }
                    break;
                case intcodemachine::TERMINATED:
                    done = true;
                    break;
            }
        }
    }


    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p21(std::cin);
}
