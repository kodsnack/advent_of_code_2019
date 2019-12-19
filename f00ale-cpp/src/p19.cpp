#include <iostream>
#include <vector>
#include <tuple>
#include <map>

#include "intcode.h"

void p19(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    const auto program = readIntcode(is);

    std::string inputstring;

    auto calcbeam=[&program] (int x, int y) {
        intcodemachine mach(program);
        mach.addInput(x);
        mach.addInput(y);
        bool done = false;
        int o = 0;
        while (!done) {
            while (mach.step());
            switch (mach.state) {
                case intcodemachine::RUNNING:
                    // do nothing, we should not end up here
                    break;
                case intcodemachine::WAITING: {
                    std::cout << "Input missing!" << std::endl;
                    done = true;
                }
                    break;
                case intcodemachine::OUTPUT:
                    o = mach.output;
                    done = true;
                    break;
                case intcodemachine::TERMINATED:
                    done = true;
                    break;
            }
        }
        return o;
    };

    std::map<std::tuple<int,int>, int> memory;
    auto getbeam = [&memory, &calcbeam](int x, int y) {
        auto t = std::tuple(x,y);
        auto it = memory.find(t);
        if(it != memory.end()) return it->second;
        auto r = calcbeam(x,y);
        memory[t] = r;
        return r;
    };

    std::vector<std::string> v;
    for(int x = 0; x < 50; x++) {
        for (int y = 0; y < 50; y++) {
            ans1 += getbeam(x,y);
        }
    }

    constexpr int N = 2000;
    constexpr int S = 100;
    int firsty = 10;
    for(int x = 10; x < N-100; x++) {
        bool seen = false;
        for(int y = firsty; y < N-100; y++) {
            auto ul = getbeam(x,y);
            if(ul && !seen) {
                seen = true;
                firsty = y;
            }
            auto ll = getbeam(x,y+S-1);
            if(seen && !ll) break;
            if(ul && ll && getbeam(x+S-1,y)) {
                ans2 = (x * 10000 + y);
                break;
            }
        }

        if(ans2) break;
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p19(std::cin);
}
