#include <iostream>
#include <vector>
#include <tuple>
#include <set>
#include <queue>

#include "intcode.h"

void p25(std::istream & is) {
    int ans1 = 0;
    std::string ans2 = "-";
    const auto program = readIntcode(is);

    std::queue<std::tuple<intcodemachine, std::set<std::string>>> q;
    std::set<std::tuple<std::string, std::set<std::string>>> visited;

    q.emplace(intcodemachine(program), std::set<std::string>());
    while(!q.empty() && !ans1) {
        auto [mach, inventory] = q.front();
        q.pop();
        bool done = false;
        std::vector<std::string> output;
        std::string currentoutput;
        while (!done) {
            while (mach.step());
            switch (mach.state) {
                case intcodemachine::RUNNING:
                    // do nothing, we should not end up here
                    break;
                case intcodemachine::WAITING: {
                    std::string room;
                    for(const auto & s : output) {
                        if(s[0] == '=') {
                            room = s;
                        }
                    }
                    if(room.empty()) {
                        done = true;
                        break;
                    }
                    if(visited.count(std::tuple(room, inventory))) {
                        done = true;
                        break;
                    }
                    visited.emplace(room, inventory);

                    std::vector<std::string> stuff, dirs;
                    for(const auto & s : output) {
                        if(s[0] == '=' && s != room) {
                            // two rooms in output
                            exit(0);
                        }
                        if(s[0] == '-') {
                            auto arg = s.substr(2);
                            if(arg == "north" || arg == "west" || arg == "south" || arg == "east") dirs.emplace_back(arg);
                            else stuff.emplace_back(arg);
                        }
                    }

                    for(const auto & dir : dirs) {
                        {
                            auto cpy = mach;
                            auto cmd = dir + '\n';
                            for (auto c : cmd) cpy.addInput(c);
                            q.emplace(cpy, inventory);
                        }
                        for(const auto & st : stuff) {
                            std::string cmd = "take " + st + '\n' + dir + '\n';
                            auto cpy = mach;
                            for(auto c : cmd) cpy.addInput(c);
                            auto icpy = inventory;
                            icpy.emplace(st);
                            q.emplace(cpy, icpy);
                        }
                    }
                }
                    done = true;
                    break;
                case intcodemachine::OUTPUT:
                    if(mach.output >= '0' && mach.output <= '9') {
                        ans1 = 10 * ans1 + (mach.output - '0');
                    } else if(ans1 > 10) {
                        done = true;
                        break;
                    } else {
                        ans1 = 0;
                    }
                    if(mach.output == 10) {
                        // this takes care of infinite loop
                        if(!output.empty() && output.back() == currentoutput) done = true;

                        if(!currentoutput.empty()) {
                            output.emplace_back(std::move(currentoutput));
                            currentoutput.clear();
                        }
                    } else {
                        currentoutput.push_back(mach.output);
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
    p25(std::cin);
}
