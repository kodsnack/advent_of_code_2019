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
    for(int problem : {1}) {
        intcodemachine mach(input);


/*        mach.addInput(4);
        while(mach.step());
        std::cout << mach.state << " " << mach.output <<std::endl;
        exit(0);
*/
        bool done = false;
        std::map<std::tuple<int, int>, char> map;

        std::queue<std::tuple<intcodemachine, int, int>> q;

        for(int i = 0; i < 4; i++) {
            auto cpy = mach;
            cpy.addInput(i+1);
            int nx = 0, ny = 0;
            switch (i+1) {
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
            if(map.find(std::tuple(nx,ny)) == map.end()) {
                q.emplace(cpy, nx, ny);
            }
        }

        int ox = 0, oy = 0;
        while (!q.empty()) {
            auto [mach, x, y] = q.front(); q.pop();
            //std::cout << "at " << x << "," << y << std::endl;
            bool done = false;
            while(!done) {
            while (mach.step());
            switch (mach.state) {
                case intcodemachine::RUNNING:
                    // do nothing, we should not end up here
                    break;
                case intcodemachine::WAITING:
                {
                    std::cout << "should not wait" << std::endl;
                }
                    break;
                case intcodemachine::OUTPUT:
                    switch(mach.output)
                    {
                        case 0: // wall
                            map[std::tuple(x,y)] = '#';
                        break;

                        case 2:
                            ox = x; oy = y; //fallthrough
                        case 1:
                            map[std::tuple(x,y)] = mach.output == 1 ? '.' : 'O';
                            for(int i = 0; i < 4; i++) {
                                auto cpy = mach;
                                cpy.addInput(i+1);
                                int nx = x, ny = y;
                                switch (i+1) {
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
                                if(map.find(std::tuple(nx,ny)) == map.end()) {
                                    q.emplace(cpy, nx, ny);
                                }

                            }
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
        int minx=0,miny=0,maxx=0,maxy=0;
        for(auto & [p,c]:map){
            auto [x,y] = p;
            if(x < minx) minx = x;
            if(y < miny) miny = y;
            if(x > maxx) maxx = x;
            if(y > maxy) maxy = y;
        }

        std::vector<std::string> canvas(maxy-miny+1);
        std::vector<std::vector<int>> dists(maxy-miny+1);
        for(auto & s : canvas) s.resize(maxy-miny+1);
        for(auto & [p,c]:map) {
            auto[x, y] = p;
            canvas[y-miny][x-minx] = c;
        }
        for(auto & s : canvas) std::cout << s << std::endl;

        {
            std::queue<std::tuple<int, int, int>> q2;
            q2.emplace(0, 0, 0);
            std::set<std::tuple<int, int>> visited;
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
                        q2.emplace(x + 1, y, d + 1);
                        q2.emplace(x - 1, y, d + 1);
                        q2.emplace(x, y + 1, d + 1);
                        q2.emplace(x, y - 1, d + 1);
                        break;
                    case 'O':
                        ans1 = d;
                        while (!q2.empty()) q2.pop();
                        break;
                    default:
                        std::cout << "unexpected " << c << " " << +c << std::endl;
                        break;
                }
            }
        }


        {
            std::queue<std::tuple<int, int, int>> q2;
            q2.emplace(ox, oy, 0);
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
                    case 'O':
                    case '.':
                        q2.emplace(x + 1, y, d + 1);
                        q2.emplace(x - 1, y, d + 1);
                        q2.emplace(x, y + 1, d + 1);
                        q2.emplace(x, y - 1, d + 1);
                        break;
                    default:
                        std::cout << "unexpected " << c << " " << +c << std::endl;
                        break;
                }
                ans2=d;
            }
        }

    }



    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p15(std::cin);
}
