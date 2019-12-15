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

void p15(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    auto input = readIntcode(is);

    intcodemachine mach(input);
    std::map<std::tuple<int, int>, char> map;
    int ox = 0, oy = 0;

    {
        std::queue<std::tuple<intcodemachine, int, int>> q;

        auto cloneMachine = [&map, &q](const intcodemachine &mach, int x, int y) {
            for (int i = 0; i < 4; i++) {
                auto cpy = mach;
                cpy.addInput(i + 1);
                int nx = x, ny = y;
                switch (i + 1) {
                    case 1:
                        ny--;
                        break;
                    case 2:
                        ny++;
                        break;
                    case 3:
                        nx--;
                        break;
                    case 4:
                        nx++;
                        break;
                }
                if (map.find(std::tuple(nx, ny)) == map.end()) {
                    q.emplace(cpy, nx, ny);
                }
            }
        };

        cloneMachine(mach, 0, 0);

        while (!q.empty()) {
            auto[mach, x, y] = q.front();
            q.pop();
            bool done = false;
            while (!done) {
                while (mach.step());
                switch (mach.state) {
                    case intcodemachine::RUNNING:
                        // do nothing, we should not end up here
                        break;
                    case intcodemachine::WAITING: {
                        std::cout << "should not wait" << std::endl;
                    }
                        break;
                    case intcodemachine::OUTPUT:
                        switch (mach.output) {
                            case 0: // wall
                                map[std::tuple(x, y)] = '#';
                                break;

                            case 2:
                                ox = x;
                                oy = y; //fallthrough
                            case 1:
                                map[std::tuple(x, y)] = mach.output == 1 ? '.' : 'O';
                                cloneMachine(mach, x, y);
                                break;
                        }
                        done = true;
                        break;
                    case intcodemachine::TERMINATED:
                        std::cout << "terminate" << std::endl;
                        done = true;
                        break;
                }
            }
        }
    }

    auto calc = [&](int sx = 0, int sy = 0, char w = 'O') {
        std::queue<std::tuple<int, int, int>> q2;
        q2.emplace(sx, sy, 0);
        std::set<std::tuple<int, int>> visited;
        int lastd = 0;
        while (!q2.empty()) {
            auto[x, y, d] = q2.front();
            q2.pop();
            if (visited.count(std::tuple(x, y))) continue;
            visited.emplace(x, y);
            auto c = map[std::tuple(x, y)];
            switch (c) {
                case '#':
                    break;
                case '.':
                case 'O':
                    q2.emplace(x + 1, y, d + 1);
                    q2.emplace(x - 1, y, d + 1);
                    q2.emplace(x, y + 1, d + 1);
                    q2.emplace(x, y - 1, d + 1);
                    break;
                default:
                    std::cout << "unexpected " << c << " " << +c << std::endl;
                    break;
            }
            if(c == w) return d;
            lastd = d;
        }
        return lastd;
    };

    ans1 = calc();
    ans2 = calc(ox, oy, '-');

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p15(std::cin);
}
