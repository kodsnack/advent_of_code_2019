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
    auto input = readIntcode(is);
    std::string output;
    intcodemachine mach(input);
    {
        bool done = false;
        mach.data[0] = 2;
        std::string input = "A,B,C\nL,12,L,10\nR,8\nL,5\ny\n";
        for(auto c : input) mach.addInput(c);

        int idx = 0;
        while (!done) {
            //bool done = false;
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
    }
    std::vector<std::string> map;
    map.push_back("");
    for(auto c : output) {
        if(c == '\n' && !map.back().empty()) map.push_back("");
        else if(c=='.' || c == '#') map.back().push_back(c);
    }

    while(map.back().empty()) map.pop_back();

    for(int i = 1; i < map.size()-1; i++) {
        for(int j = 1; j < map[i].length()-1; j++) {
            if(map[i][j] == '#') {
                if(map[i][j+1] == '#' && map[i][j-1] == '#' && map[i+1][j] == '#' && map[i-1][j] == '#') {
                    std::cout << i << " " << j << " " << (i*j) << std::endl;
                    ans1 += i*j;
                }
            }
        }
        std::cout << i << " " << map[i] << std::endl;
    }


    std::cout << output << std::endl;

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p17(std::cin);
}
