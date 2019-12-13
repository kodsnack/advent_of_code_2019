#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <map>

#include "intcode.h"

void p11(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    auto input = readIntcode(is);

    for(int problem : {1,2}) {
        intcodemachine mach(input);
        bool done = false;
        int x = 0, y = 0;
        int dx = 0, dy = -1;
        std::map<std::tuple<int, int>, int> painted;
        std::vector<int> outputs;
        if(problem == 2) {
            mach.data[0] = 2;
        }

        auto draw = [&]() {
            int minx = std::numeric_limits<int>::max();
            int miny = std::numeric_limits<int>::max();
            int maxx = std::numeric_limits<int>::min();
            int maxy = std::numeric_limits<int>::min();
            for (auto[t, c] : painted) {
                auto[x, y] = t;
                if (x < minx) minx = x;
                if (y < miny) miny = y;
                if (x > maxx) maxx = x;
                if (y > maxy) maxy = y;
            }

            auto xs = maxx - minx + 1;
            auto ys = maxy - miny + 1;
            std::vector<std::string> screen;
            screen.resize(ys);
            for (auto &v : screen) v.resize(xs, ' ');
            for( auto & [t,c] : painted) {
                auto [x,y] = t;
                switch (c) {
                    case 1: screen[y][x] = '|'; break;
                    case 2: screen[y][x] = 'X'; break;
                    case 3: screen[y][x] = '-'; break;
                    case 4: screen[y][x] = '0'; break;
                }
            }
            for (auto &v : screen) std::cout << v << std::endl;

        };

        while (!done) {
            while (mach.step());
            switch (mach.state) {
                case intcodemachine::RUNNING:
                    // do nothing, we should not end up here
                    break;
                case intcodemachine::WAITING:
                {
                    draw();
                    mach.addInput(0);
                }
                    break;
                case intcodemachine::OUTPUT:
                    outputs.push_back(mach.output);
                    if(outputs.size() == 3) {
                        if(outputs[0] == -1 && outputs[1] == 0) {
                            ans2 = outputs[2];
                        } else {
                            painted[std::tuple(outputs[0],outputs[1])] = outputs[2];
                        }
                        outputs.clear();
                    }

                    break;
                case intcodemachine::TERMINATED:
                    done = true;
                    break;
            }
        }
        if(problem == 1) {
            for(auto & [t,b] : painted) {
                if(b == 2) ans1++;
            }

        } else {
        }
    }
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p11(std::cin);
}
