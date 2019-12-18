#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <set>
#include <map>
#include <queue>

void p18(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    std::vector<std::string> map;
    {
        bool done = false;

        bool newline = true;
        while (!done) {
            char c;
            is.get(c);
            if (!is.good()) {
                done = true;
                c = '\n';
            }


            if(c == '\n') {
                newline = true;
            } else {
                if(newline) {
                    map.push_back("");
                    newline=false;
                }
                map.back().push_back(c);
            }

        }
    }

    int i = 0;
    int sx = 0, sy = 0, nkeys = 0;
    for(auto & s : map) {
        int px = 0;
        for(auto c : s) {
            if(c=='@') {
                sx = px;
                sy = i;
            }
            if(islower(c)) nkeys++;
            px++;
        }
        i++;
    }
    std::cout << sx << " " << sy << " " << nkeys << std::endl;

    auto buildreachable = [](int sx, int sy, const std::set<char> & keys, const std::vector<std::string> & map){
        std::vector<std::tuple<int, int, char, int>> ret;
        std::queue<std::tuple<int,int,int>> q;
        q.emplace(std::tuple(sx,sy,0));
        std::set<char> found;
        std::set<std::tuple<int,int>> visited;
        while(!q.empty()) {
            auto [x,y,step] = q.front(); q.pop();
            char c = map[y][x];
            if(c == '#') continue;
            if(visited.find(std::tuple(x,y)) != visited.end()) continue;
            visited.insert(std::tuple(x,y));
            if(islower(c)) {
                if(keys.find(map[y][x]) == keys.end() && found.find(c) == found.end()) {
                    // new key
                    found.insert(c);
                    ret.emplace_back(x,y, map[y][x], step);
                }
            }
            if(isupper(map[y][x]) && keys.find(tolower(map[y][x])) == keys.end()) continue;
            q.emplace(x+1, y, step+1);
            q.emplace(x-1, y, step+1);
            q.emplace(x, y-1, step+1);
            q.emplace(x, y+1, step+1);
        }
        return ret;
    };
/*
    std::set<char> k;
    auto v = buildreachable(sx,sy,k,map);
    for(auto & [x,y,k,s] : v) {
        std::cout << k << " " << s << std::endl;
    }
*/
    std::queue<std::tuple<int, int, int, std::set<char>>> q;
    q.emplace(sx,sy,0,std::set<char>());

    ans1 = std::numeric_limits<int>::max();
    while(!q.empty()) {
        auto [x,y,step,keys] = q.front(); q.pop();
        //std::cout << step << std::endl;
        auto reachabe = buildreachable(x,y,keys,map);
        for(auto [nx, ny, nk, ns] : reachabe) {
            auto keycopy = keys;
            keycopy.insert(nk);
            if(keycopy.size() == nkeys) {
                ans1 = std::min(step+ns,ans1);
                std::cout << ans1 << std::endl;
            }
            q.emplace(nx,ny,step+ns,keycopy);
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p18(std::cin);
}
