#include <iostream>
#include <vector>
#include <tuple>
#include <queue>
#include <algorithm>
#include <numeric>

#include "intcode.h"

void p07(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;

    const auto input = readIntcode(is);

    auto ans = [&](int prob) -> auto & { return prob == 0 ? ans1 : ans2 ;};
    for(int i = 0; i < 2; i++) {
        std::vector<int> phase(5);
        std::iota(phase.begin(), phase.end(), i*5);

        do {
            std::vector<intcodemachine> mach(5, intcodemachine(input));
            for(int i = 0; i < 5; i++) {
                mach[i].addInput(phase[i]);
            }
            mach[0].addInput(0);

            while(mach[4].state != intcodemachine::TERMINATED) {
                for(int i = 0; i < 5; i++) {
                    while(mach[i].step());
                    mach[(i+1)%5].addInput(mach[i].output);
                }
            }
            if(mach[4].output > ans(i)) ans(i) = mach[4].output;
        } while (std::next_permutation(phase.begin(), phase.end()));
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p07(std::cin);
}
