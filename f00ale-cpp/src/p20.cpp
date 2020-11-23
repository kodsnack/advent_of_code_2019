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

    std::map<std::array<char,2>, std::tuple<int,int>> endpointstmp;

    std::map<std::tuple<int,int>, std::tuple<int,int>> portals;
    std::map<std::tuple<int,int>,std::array<char,2>> portalnames;

    for(size_t y = 0; y < map.size(); y++) {
        for(size_t x = 0; x < map[y].size(); x++) {
            if(y+1 < map.size() && isupper(map[y][x]) && isupper(map[y+1][x])) {
                // vertical portal
                int ry;
                if(y > 0 && map[y-1][x] == '.') ry = y-1;
                else if(y+2 < map.size() && map[y+2][x] == '.') ry = y+2;
                else { std::cout << "no portal y " << std::endl; exit(0); }
                std::array<char, 2> n{map[y][x],map[y+1][x]};
                portalnames.emplace(std::tuple(x,ry),n);
                auto it = endpointstmp.find(n);
                if(it != endpointstmp.end()) {
                    auto [ox,oy] = it->second;
                    endpointstmp.erase(it);
                    portals.emplace(std::tuple(x,ry), std::tuple(ox,oy));
                    portals.emplace(std::tuple(ox,oy), std::tuple(x,ry));
                } else {
                    endpointstmp.emplace(n, std::tuple(x,ry));
                }
            }
            if(x+1 < map[y].size() && isupper(map[y][x]) && isupper(map[y][x+1])) {
                // horizontal portal
                int rx;
                if(x > 0 && map[y][x-1] == '.') rx = x-1;
                else if(x+2 < map[y].size() && map[y][x+2] == '.') rx = x+2;
                else { std::cout << "no portal x " << std::endl; exit(0); }
                std::array<char, 2> n{map[y][x],map[y][x+1]};
                portalnames.emplace(std::tuple(rx,y),n);

                auto it = endpointstmp.find(n);
                if(it != endpointstmp.end()) {
                    auto [ox,oy] = it->second;
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
    auto goal = endpointstmp[std::array<char,2>{'Z','Z'}];

    for(auto [t1,t2] : portalnames) {
        auto [x,y] = t1;
        map[y][x] = 'P';
    }

    for(auto problem : {1,2}) {
        std::queue<std::tuple<std::tuple<int, int>, int, bool, int>> q;
        q.emplace(std::tuple(start, 0, true, 0));
        std::set<std::tuple<int, int, int>> visited;
        while (!q.empty()) {
            auto[p, step, teleported, level] = q.front();
            q.pop();
            if (level < 0) continue;
            auto[x, y] = p;
            if (p == goal && level == 0) {
                (problem == 1 ? ans1 : ans2) = step;
                break;
            }
            auto pp = std::tuple(x, y, level);
            if (visited.count(pp)) continue;
            visited.emplace(pp);

            if (map[y][x] == '.' || teleported) {
                q.emplace(std::tuple(x + 1, y), step + 1, false, level);
                q.emplace(std::tuple(x - 1, y), step + 1, false, level);
                q.emplace(std::tuple(x, y + 1), step + 1, false, level);
                q.emplace(std::tuple(x, y - 1), step + 1, false, level);
            } else if (map[y][x] == 'P') {
                // portal
                auto it = portals.find(p);
                if (it != portals.end()) {
                    int ld = 0;
                    if(problem == 2) {
                        if (x == 2 || x == static_cast<int>(map[y].size()) - 3 || y == 2 || y == static_cast<int>(map.size()) - 3) ld = -1;
                        else ld = 1;
                    }
                    q.emplace(it->second, step + 1, true, level + ld);
                }
            }
        }
    }
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p20(std::cin);
}
