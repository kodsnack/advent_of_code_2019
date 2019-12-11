#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <map>

#include "intcode.h"

void p11(std::istream & is) {
    int ans1 = 0;
    std::vector<std::string> ans2;

    auto input = readIntcode(is);

    for(int problem : {1,2}) {
        intcodemachine mach(input);
        bool done = false;
        int x = 0, y = 0;
        int dx = 0, dy = -1;
        std::map<std::tuple<int, int>, char> painted;
        int out1 = 0, out2 = 0;
        bool has1 = false;
        if(problem == 2) {
            painted[std::tuple(0, 0)] = '#';
        }
        while (!done) {
            while (mach.step());
            switch (mach.state) {
                case intcodemachine::RUNNING:
                    // do nothing, we should not end up here
                    break;
                case intcodemachine::WAITING: {
                    auto it = painted.find(std::tuple(x, y));
                    char col = '.';
                    if (it != painted.end()) col = it->second;
                    mach.addInput(col == '#' ? 1 : 0);
                }
                    break;
                case intcodemachine::OUTPUT:
                    if (has1) {
                        out2 = mach.output;
                        has1 = false;
                        painted[std::tuple(x, y)] = (out1 ? '#' : '.');
                        bool right = out2;
                        if (dy < 0) {
                            dy = 0;
                            dx = right ? 1 : -1;
                        } else if (dy > 0) {
                            dy = 0;
                            dx = right ? -1 : 1;
                        } else if (dx < 0) {
                            dx = 0;
                            dy = right ? -1 : 1;
                        } else {
                            dx = 0;
                            dy = right ? 1 : -1;
                        }
                        x += dx;
                        y += dy;
                    } else {
                        out1 = mach.output;
                        has1 = true;
                    }
                    break;
                case intcodemachine::TERMINATED:
                    done = true;
                    break;
            }
        }
        if(problem == 1) {
            ans1 = painted.size();
        } else {
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
            ans2.resize(ys);
            for (auto &v : ans2) v.resize(xs, ' ');
            for (auto[t, c] : painted) {
                auto[x, y] = t;
                if(c == '#') ans2[y - miny][x - miny] = '*';
            }
        }
    }
    std::cout << ans1 << std::endl;
    for (auto &s : ans2) {
        std::cout << s << std::endl;
    }
}

int main() {
    p11(std::cin);
}
