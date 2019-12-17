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
            //std::string input = "B,A,B,A,C,C,A,B,A,C\nR,8,R,10,R,12\nL,12,L,10,R,8,L,12\nL,10,R,12,R,8\nn\n";
            for (auto c : inputstring) mach.addInput(c);
        }

        while (!done) {
            while (!done) {
                while (mach.step());
                switch (mach.state) {
                    case intcodemachine::RUNNING:
                        // do nothing, we should not end up here
                        break;
                    case intcodemachine::WAITING:
                    {
                        std::cout << "should not wait" << std::endl;
                        done = true;
                    }
                        break;
                    case intcodemachine::OUTPUT:
                        if(mach.output > 127) ans2 = mach.output;
                        else output.push_back((char)mach.output);
                        break;
                    case intcodemachine::TERMINATED:
                        std::cout << "terminate" << std::endl;
                        done = true;
                        break;
                }
            }
        }

        if(problem == 1) {
            map.push_back("");
            for (auto c : output) {
                if (c == '\n' && !map.back().empty()) map.push_back("");
                else if (c == '.' || c == '#') map.back().push_back(c);
                else if (c == '<' || c == '>' || c == '^' || c == 'v') {
                    px = map.back().length();
                    py = map.size()-1;
                    map.back().push_back('#');
                    if(c == '<') { dx = -1; dy = 0; }
                    if(c == '>') { dx = 1; dy = 0; }
                    if(c == '^') { dx = 0; dy = -1; }
                    if(c == 'v') { dx = 0; dy = 1; }
                }
            }

            while (map.back().empty()) map.pop_back();

            std::cout << "   ";
            for(int i = 0; i < 50; i++) std::cout << i/10;
            std::cout << "\n   ";
            for(int i = 0; i < 50; i++) std::cout << i%10;
            std::cout << "\n";
            for (int i = 1; i < map.size() - 1; i++) {
                for (int j = 1; j < map[i].length() - 1; j++) {
                    if (map[i][j] == '#') {
                        if (map[i][j + 1] == '#' && map[i][j - 1] == '#' && map[i + 1][j] == '#' &&
                            map[i - 1][j] == '#') {
                            ans1 += i * j;
                        }
                    }
                }
                std::cout << i << (i<10?"  ":" ") << map[i] << std::endl;
            }

            //std::cout << output << std::endl;
            bool done = false;
            char LD = 'X';
            char id = 'd';
            std::map<std::string, char> ids;
            std::string cmd;
            while(!done) {
                int steps = 0;
                int nx = px + dx;
                int ny = py + dy;
                while (nx >= 0 && nx < map[py].length() && ny >= 0 && ny < map.size()
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
                    if(ids.find(tmp) == ids.end()) ids[tmp] = id++;
                    std::cout << ids[tmp] << " " << tmp << std::endl;
                    cmd.push_back(LD);
                    cmd.push_back('a'+steps);
                }

                done = true;
                //for(auto & s : map) std::cout << s << std::endl;
                if (dy) {
                    if (px - 1 > 0) { // can turn
                        if (map[py][px-1] == '#') {
                            LD = (dy<0?'L':'R');
                            done = false;
                            dx = -1;
                        }
                    }
                    if (px + 1 < map[py].size()) { //can turn
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
                    if (py + 1 < map.size()) { //can turn
                        if(map[py+1][px] == '#') {
                            LD = (dx<0?'L':'R');
                            done = false;
                            dy = 1;
                        }
                    }
                    dx = 0;
                }
            }
            std::cout << cmd << std::endl;

            auto len = [](const std::string & s) {
                int ret = s.length()-1; // number of L/R and commas
                for(int i = 0; i < s.length(); i++) {
                    if(s[i]-'a' >= 10) ret += 2;
                    else ret++;
                }
                return ret;
            };

            std::array<std::string, 3> strs;
            for(int l1 = 2; l1*2 < cmd.length(); l1++) {
                strs[0] = cmd.substr(0, 2 * l1);
                if(len(strs[0]) > 20) break;
                if(strs[0].length() < 2 * l1) break;
                for(int l2 = 2; (l1+l2)*2 < cmd.length(); l2++) {
                    auto startpos2 = strs[0].length();
                    while(strs[0] == cmd.substr(startpos2, strs[0].length())) startpos2 += strs[0].length();
                    strs[1] = cmd.substr(startpos2, l2 * 2);
                    if(strs[1].length() < 2 * l2) break;
                    if(len(strs[1]) > 20) break;

                    for(int l3 = 2; (l1+l2+l3)*2 < cmd.length(); l3++) {
                        auto startpos3 = startpos2 + strs[1].length();
                        auto old = startpos3;
                        do {
                            old = startpos3;
                            if(strs[0] == cmd.substr(startpos3, strs[0].length())) startpos3 += strs[0].length();
                            if(strs[1] == cmd.substr(startpos3, strs[1].length())) startpos3 += strs[1].length();
                        } while(old != startpos3);
                        strs[2] = cmd.substr(startpos3, 2 * l3);
                        if(len(strs[2]) > 20 || strs[2].length() < 2 * l3) break;
                        //std::cout << l1 << " " << l2 << " " << l3 << " " << s[0] << " .. " << s2 << " : " << len(s[0]) << " " << len(s2) << std::endl;
                        std::cout << strs[0] << " .. " << strs[1] << " .. " << strs[2] << std::endl;

                        int pos = 0;
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
                            std::cout << "MATCH " << seq << std::endl;
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
                            inputstring = instr;
                        }
                    }
                }
            }

        }
    }
    std::cout << std::endl;

    std::cout << px << " " << py << " " << dx << " " << dy << std::endl;

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p17(std::cin);
}
