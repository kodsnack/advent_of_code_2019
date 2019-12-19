#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <map>
#include <stack>
#include <queue>

#include "intcode.h"
#include <fstream>
#include <set>

void p17(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    const auto program = readIntcode(is);

    std::string inputstring;

    auto getbeam=[&program] (int x, int y) {
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

    std::vector<std::string> v;
    for(auto problem : {1}){
        std::string output;

        for(int x = 0; x < 50; x++) {
            for (int y = 0; y < 50; y++) {
                ans1 += getbeam(x,y);
            }
        }
        constexpr int N = 2000;
        for(int x = 0; x < N; x++) {
            v.push_back("");
            int l = 0;
            for(int y = 0; y < N; y++) {
                auto t = getbeam(x,y);
                if(t)l++;
                v.back().push_back(t?'#':' ');
            }
            //std::cout << l << std::endl;
        }

        std::vector<std::vector<int>> vx(N);
        for(int x = 0; x < N; x++) {
            int acc = 0;
            vx[x].resize(N);
            for (int y = 0; y < N; y++) {
                acc += v[x][y] == '#';
                vx[x][y] = acc;
            }
        }

        for(int x = 10; x < N-100; x++) {
            for(int y = 10; y < N-100; y++) {
                if(v[x][y] == '#' && v[x+99][y] == '#' && v[x][y+99] == '#') {
                    ans2 = (x * 10000 + y);
                    break;
                }
            }

            if(ans2) break;
        }
    }
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p17(std::cin);
}
