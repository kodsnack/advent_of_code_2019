#include <iostream>

#include "intcode.h"

void p09(std::istream & is) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;

    const auto input = readIntcode(is);

    auto ans = [&](int prob) -> auto & { return prob == 1 ? ans1 : ans2 ;};

    for(int problem : {1,2}) {
        intcodemachine mach(input);
        mach.addInput(problem);
        while(mach.step());
        ans(problem) = mach.output;
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p09(std::cin);
}
