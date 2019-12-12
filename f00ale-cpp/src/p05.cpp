#include <iostream>

#include "intcode.h"

void p05(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;

    const auto input = readIntcode(is);

    auto progdata = [](int prob) { return prob == 1 ? 1 : 5; };

    auto ans = [&](int prob) -> auto & { return prob == 1 ? ans1 : ans2 ;};

    for(const auto problem : {1,2}) {
        intcodemachine mach(input);
        mach.addInput(progdata(problem));
        do {
            while(mach.step());
            if(mach.state == intcodemachine::WAITING) mach.addInput(progdata(problem));
        } while(mach.state != intcodemachine::TERMINATED);

        ans(problem) = mach.output;
    }
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p05(std::cin);
}
