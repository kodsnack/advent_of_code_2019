#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <set>
#include <map>
#include <queue>

void p20(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;

    std::vector<std::string> map;

    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        std::string str;

        while (!done) {
            char c;
            is.get(c);
            if (!is.good()) {
                done = true;
                c = '\n';
            }

            if(c == '\n') {
                if(!str.empty()) map.push_back(str);
                str.clear();
            } else {
                str.push_back(c);
            }

        }
    }

    for(auto & s : map) std::cout << s << std::endl;

    std::map<std::array<char,2>, std::tuple<int,int>> endpointstmp;

    std::map<std::tuple<int,int>, std::tuple<int,int>> portals;
    std::map<std::tuple<int,int>,std::array<char,2>> portalnames;

    for(int y = 0; y < map.size(); y++) {
        for(int x = 0; x < map[y].size(); x++) {
            if(y+1 < map.size()) if(isupper(map[y][x]) && isupper(map[y+1][x])) {
                // vertical portal
                //std::cout << x << " " << y << "," << y+1 << " " << map[y][x]<<map[y+1][x] <<std::endl;
                int ry;
                if(y > 0 && map[y-1][x] == '.') ry = y-1;
                else if(y+2 < map.size() && map[y+2][x] == '.') ry = y+2;
                else { std::cout << "no portal y " << std::endl; exit(0); }
                std::array<char, 2> n{map[y][x],map[y+1][x]};
                portalnames.emplace(std::tuple(x,ry),n);
                auto it = endpointstmp.find(n);
                if(it != endpointstmp.end()) {
                    auto [ox,oy] = it->second;
                    //std::cout << x << ',' << ry << " - " << ox << ',' << oy << std::endl;
                    endpointstmp.erase(it);
                    portals.emplace(std::tuple(x,ry), std::tuple(ox,oy));
                    portals.emplace(std::tuple(ox,oy), std::tuple(x,ry));
                } else {
                    endpointstmp.emplace(n, std::tuple(x,ry));
                }
            }
            if(x+1 < map[y].size()) if(isupper(map[y][x]) && isupper(map[y][x+1])) {
                // horizontal portal
                //std::cout << x << "," << x+1 << " " << y << " " << map[y][x]<<map[y][x+1] <<std::endl;
                int rx;
                if(x > 0 && map[y][x-1] == '.') rx = x-1;
                else if(x+2 < map[y].size() && map[y][x+2] == '.') rx = x+2;
                else { std::cout << "no portal x " << std::endl; exit(0); }
                std::array<char, 2> n{map[y][x],map[y][x+1]};
                portalnames.emplace(std::tuple(rx,y),n);

                auto it = endpointstmp.find(n);
                if(it != endpointstmp.end()) {
                    auto [ox,oy] = it->second;
                    //std::cout << rx << ',' << y << " - " << ox << ',' << oy << std::endl;
                    endpointstmp.erase(it);
                    portals.emplace(std::tuple(rx,y), std::tuple(ox,oy));
                    portals.emplace(std::tuple(ox,oy), std::tuple(rx,y));

                } else {
                    endpointstmp.emplace(n, std::tuple(rx,y));
                }
            }
        }
    }

    auto start = endpointstmp[std::array<char,2>{'A','A'}];
    auto [sx,sy] = start;
    std::cout << sx << ',' << sy << std::endl;
    auto goal = endpointstmp[std::array<char,2>{'Z','Z'}];

    for(auto [t1,t2] : portalnames) {
        auto [x,y] = t1;
        map[y][x] = 'P';
    }
    //map[sy][sx] = '.';
    for(auto & s : map) std::cout << s << std::endl;

    std::queue<std::tuple<std::tuple<int,int>, int, bool>> q;
    q.emplace(std::tuple(start, 0, true));
    std::set<std::tuple<int,int>> visited;
    while(!q.empty()) {
        auto [p, step, teleported] = q.front();
        q.pop();
        auto [x,y] = p;
        if(p == goal) {
            ans1 = step;
            break;
        }
        if(visited.count(p)) continue;
        visited.emplace(p);

        if(map[y][x] == '.' || teleported) {
            q.emplace(std::tuple(x+1,y), step+1, false);
            q.emplace(std::tuple(x-1,y), step+1, false);
            q.emplace(std::tuple(x,y+1), step+1, false);
            q.emplace(std::tuple(x,y-1), step+1, false);
        } else if(map[y][x]=='P') {
            // portal
            auto it = portals.find(p);
            if(it == portals.end()) {
                std::cout << "portal endpoint not found" << std::endl;
            } else {
                q.emplace(it->second, step+1, true);
                auto [nx,ny] = it->second;
                std::cout << " teleport " << x << ',' << y << " -> " << nx << ',' << ny << std::endl;
            }
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p20(std::cin);
}
