#include <iostream>
#include <vector>
#include <numeric>

#include "intcode.h"

void p23(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    const auto program = readIntcode(is);

    std::vector<intcodemachine> machs(50, intcodemachine(program));
    for(int i = 0; i < 50; i++) machs[i].addInput(i);
    std::vector<std::queue<int64_t>> inputs(50);
    std::vector<std::vector<int64_t>> outputs(50);
    int64_t natx = 0, naty = 0;
    std::vector<int> hasaction(50);
    int64_t lastnaty = 0;
    bool done = false;

    while (!done) {
        for (int idx = 0; idx < 50; idx++) {
            auto &mach = machs[idx];
            mach.step();
            switch (mach.state) {
                case intcodemachine::RUNNING:
                    break;
                case intcodemachine::WAITING:
                    if (inputs[idx].empty()) {
                        mach.addInput(-1);
                        hasaction[idx] = 1;
                    } else {
                        mach.addInput(inputs[idx].front());
                        inputs[idx].pop();
                        hasaction[idx] = 0;
                    }
                    break;
                case intcodemachine::OUTPUT:
                    hasaction[idx] = 0;
                    outputs[idx].push_back(mach.output);
                    if (outputs[idx].size() == 3) {
                        auto to = outputs[idx][0];
                        auto x = outputs[idx][1];
                        auto y = outputs[idx][2];
                        outputs[idx].clear();
                        if (to == 255) {
                            if(!ans1) ans1 = y;
                            natx = x;
                            naty = y;
                        } else {
                            inputs[to].emplace(x);
                            inputs[to].emplace(y);
                        }
                    }
                    break;
                case intcodemachine::TERMINATED:
                    done = true;
                    break;
            }
        }

        auto idle_count = std::accumulate(hasaction.begin(), hasaction.end(), 0);
        if(idle_count == 50 && (natx || naty)) {
            if(naty == lastnaty) {
                ans2 = naty;
                done = true;
            }
            inputs[0].push(natx);
            inputs[0].push(naty);
            lastnaty = naty;
            natx = naty = 0;
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p23(std::cin);
}
