#include <iostream>
#include <vector>
#include <tuple>
#include <set>
#include <queue>
#include <map>
#include <unordered_set>

#include "intcode.h"

void p25(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    const auto program = readIntcode(is);

    std::queue<std::tuple<intcodemachine, std::set<std::string>,std::string>> q;
    std::set<std::tuple<std::string, std::set<std::string>>> visited;

    auto splitstring = [](const std::string & s) {
        std::vector<std::string> ret;
        std::string::size_type pos = 0;
        auto n = s.find('\n');
        while(n != std::string::npos) {
            if(n-pos > 0)
            ret.push_back(s.substr(pos, n-pos));
            pos = n + 1;

            n = s.find('\n', pos);
        }
        return ret;
    };

    const std::unordered_set<std::string> forbidden {
        "infinite loop",
        "molten lava",
        "escape pod",
        "giant electromagnet",
        "photons"
    };

    q.emplace(intcodemachine(program), std::set<std::string>(), "");
    while(!q.empty()) {
        auto [mach, inventory, last] = q.front();
        q.pop();
        bool done = false;
        std::string output;
        while (!done) {
            while (mach.step());
            switch (mach.state) {
                case intcodemachine::RUNNING:
                    // do nothing, we should not end up here
                    break;
                case intcodemachine::WAITING:
                {
                    //std::cout << output << std::endl;
                    auto strs = splitstring(output);
                    for(auto & s : strs){
                        if(s == "Unrecognized command.") {
                            std::cout << s << std::endl;
                            std::cout << last << std::endl;
                            exit(0);
                        }
                    }
                    if(strs.size() == 0) {
                        done = true;
                        break;
                    }
                    std::string room;
                    for(auto & s : strs) {
                        if(s[0] == '=') {
                            room = s;
                        }
                    }
                    if(room.empty()) {
                        std::cout << "xxx" << output << std::endl;
                        std::cout << "yyy" << last << std::endl;
                        exit(0);
                        break;
                    }
                    if(visited.count(std::tuple(room, inventory))) {
                        done = true;
                        break;
                    }
                    visited.emplace(room, inventory);


                    std::cout << room;
                    for(auto & i : inventory) std::cout << " " << i;
                    std::cout << std::endl;

                    std::vector<std::string> stuff, dirs;
                    for(const auto & s : strs) {
                        if(s[0] == '=' && s != room) {
                            std::cout << "two rooms!" << std::endl;
                            stuff.clear();
                            dirs.clear();
                            exit(0);
                        }
                        if(s[0] == '-') {
                            auto arg = s.substr(2);
                            if(arg == "north" || arg == "west" || arg == "south" || arg == "east") dirs.emplace_back(arg);
                            else stuff.emplace_back(arg);
                        }
                    }

                    for(auto & dir : dirs) {
                        {
                            auto cpy = mach;
                            auto cmd = dir + "\n";
                            for (auto c : cmd) cpy.addInput(c);
                            q.emplace(cpy, inventory, output + dir);
                        }
                        for(auto & st : stuff) {
                            if(inventory.find(st) != inventory.end()) continue;
                            if(forbidden.find(st) != forbidden.end()) continue;
                            std::string cmd = "take " + st + "\n" + dir + "\n";
                            auto cpy = mach;
                            for(auto c : cmd) cpy.addInput(c);
                            auto icpy = inventory;
                            icpy.emplace(st);
                            q.emplace(cpy, icpy, output + cmd);
                        }
                    }


                    //
//                    auto cpy = mach;
//                    for(auto c : arg) {
//                        cpy.addInput(c);
//                    }
//                    cpy.addInput(10);
//                    q.emplace(cpy, inventory);


                }   done = true;
                    break;
                case intcodemachine::OUTPUT:
                    output.push_back(mach.output);
                    if(output.size() > 5000) {
                        std::cout << "long output" << std::endl;
                        std::cout << last << std::endl;
                        std::cout << ":::::" << std::endl;
                        std::cout << output << std::endl;
                        exit(0);
                    }
                    break;
                case intcodemachine::TERMINATED:

                    std::cout <<"terminated" << std::endl;
                    std::cout << last << std::endl;
                    std::cout <<"<>>" << std::endl;
                    std::cout << output << std::endl;
                    std::cout <<"----" << std::endl;
                    done = true;
                    break;
            }
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

#include <fstream>
int main() {
    //p25(std::cin);exit(0);
    std::ifstream is("/Users/arno/git/aoc19/f00ale-cpp/data/p25.txt");
    p25(is);
}
