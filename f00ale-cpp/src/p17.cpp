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
    const auto input = readIntcode(is);
    std::vector<std::string> map;
    int px = 0, py = 0, dx = 0, dy = 0;

    std::string inputstring;

    for(auto problem : {1,2}){
        std::string output;
        intcodemachine mach(input);
        bool done = false;
        mach.data[0] = problem;

        if(problem == 2) {
            bool done = false;
            char LD = 'X';
            std::string cmd;
            while(!done) {
                int steps = 0;
                int nx = px + dx;
                int ny = py + dy;
                while (nx >= 0 && nx < static_cast<int>(map[py].length()) &&
                       ny >= 0 && ny < static_cast<int>(map.size())
                       && (map[ny][nx] == '#' || map[ny][nx] == '+')) {
                    map[ny][nx] = '+';
                    px += dx;
                    py += dy;
                    nx += dx;
                    ny += dy;
                    steps++;
                }
                // at end, turn
                if(steps) {
                    auto tmp = LD + std::to_string(steps);
                    cmd.push_back(LD);
                    cmd.push_back('a'+steps);
                }

                done = true;
                if (dy) {
                    if (px - 1 > 0) { // can turn
                        if (map[py][px-1] == '#') {
                            LD = (dy<0?'L':'R');
                            done = false;
                            dx = -1;
                        }
                    }
                    if (px + 1 < static_cast<int>(map[py].size())) { //can turn
                        if(map[py][px+1] == '#') {
                            LD = (dy<0?'R':'L');
                            done = false;
                            dx = 1;
                        }
                    }
                    dy = 0;
                } else { // dx != 0
                    if (py - 1 > 0) { // can turn
                        if (map[py-1][px] == '#') {
                            LD = (dx<0?'R':'L');
                            done = false;
                            dy = -1;
                        }
                    }
                    if (py + 1 < static_cast<int>(map.size())) { //can turn
                        if(map[py+1][px] == '#') {
                            LD = (dx<0?'L':'R');
                            done = false;
                            dy = 1;
                        }
                    }
                    dx = 0;
                }
            }

            auto len = [](const std::string & s) {
                int ret = s.length()-1; // number of L/R and commas
                for(auto c : s) {
                    if(c-'a' >= 10) ret += 2;
                    else ret++;
                }
                return ret;
            };

            std::array<std::string, 3> strs;
            for(size_t l1 = 2; l1*2 < cmd.length(); l1++) {
                strs[0] = cmd.substr(0, 2 * l1);
                if(len(strs[0]) > 20) break;
                if(strs[0].length() < 2 * l1) break;
                for(size_t l2 = 2; (l1+l2)*2 < cmd.length(); l2++) {
                    auto startpos1 = strs[0].length();
                    while(strs[0] == cmd.substr(startpos1, strs[0].length())) startpos1 += strs[0].length();
                    strs[1] = cmd.substr(startpos1, l2 * 2);
                    if(strs[1].length() < 2 * l2) break;
                    if(len(strs[1]) > 20) break;

                    for(size_t l3 = 2; (l1+l2+l3)*2 < cmd.length(); l3++) {
                        auto startpos3 = startpos1 + strs[1].length();
                        auto old = startpos3;
                        do {
                            old = startpos3;
                            if(strs[0] == cmd.substr(startpos3, strs[0].length())) startpos3 += strs[0].length();
                            if(strs[1] == cmd.substr(startpos3, strs[1].length())) startpos3 += strs[1].length();
                        } while(old != startpos3);
                        strs[2] = cmd.substr(startpos3, 2 * l3);
                        if(len(strs[2]) > 20 || strs[2].length() < 2 * l3) break;

                        std::string::size_type pos = 0;
                        bool done = false;
                        std::string seq;
                        while(!done) {
                            done = true;
                            for(int i = 0; i < 3; i++) {
                                if(strs[i] == cmd.substr(pos, strs[i].length())) {
                                    pos += strs[i].length();
                                    done = false;
                                    seq.push_back('A'+i);
                                }
                            }
                        }
                        if(pos == cmd.length()) {
                            std::string instr;
                            std::string tmp;
                            for(auto c : seq) {
                                if(!tmp.empty()) tmp.push_back(',');
                                tmp.push_back(c);
                            }
                            instr = tmp + '\n';
                            tmp.clear();
                            for(auto & s : strs) {
                                for(auto c : s) {
                                    if(!tmp.empty()) tmp.push_back(',');
                                    if(c == 'R' || c == 'L') tmp.push_back(c);
                                    else {
                                        tmp.append(std::to_string(c-'a'));
                                    }
                                }
                                instr += tmp + '\n';
                                tmp.clear();
                            }
                            instr += "n\n";
                            for (auto c : instr) mach.addInput(c);
                        }
                    }
                }
            }

        }

        while (!done) {
            while (mach.step());
            switch (mach.state) {
                case intcodemachine::RUNNING:
                    // do nothing, we should not end up here
                    break;
                case intcodemachine::WAITING:
                {
                    std::cout << "Input missing!" << std::endl;
                    done = true;
                }
                    break;
                case intcodemachine::OUTPUT:
                    if(problem == 2 && mach.output > 127) ans2 = mach.output;
                    else if(problem == 1) output.push_back((char)mach.output);
                    break;
                case intcodemachine::TERMINATED:
                    done = true;
                    break;
            }
        }

        if(problem == 1) {
            // create map
            map.push_back("");
            for (auto c : output) {
                if (c == '\n' && !map.back().empty()) map.push_back("");
                else if (c == '.' || c == '#') map.back().push_back(c);
                else if (c == '<' || c == '>' || c == '^' || c == 'v') {
                    px = map.back().length();
                    py = map.size() - 1;
                    map.back().push_back('#');
                    if (c == '<') {
                        dx = -1;
                        dy = 0;
                    }
                    if (c == '>') {
                        dx = 1;
                        dy = 0;
                    }
                    if (c == '^') {
                        dx = 0;
                        dy = -1;
                    }
                    if (c == 'v') {
                        dx = 0;
                        dy = 1;
                    }
                }
            }

            while (map.back().empty()) map.pop_back();

            // calculate answer 1
            for (int i = 1; i < static_cast<int>(map.size() - 1); i++) {
                for (int j = 1; j < static_cast<int>(map[i].length() - 1); j++) {
                    if (map[i][j] == '#') {
                        if (map[i][j + 1] == '#' && map[i][j - 1] == '#' && map[i + 1][j] == '#' &&
                            map[i - 1][j] == '#') {
                            ans1 += i * j;
                        }
                    }
                }
            }
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p17(std::cin);
}
