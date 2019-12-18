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

    std::queue<std::tuple<int, int, int, std::set<char>, std::set<std::tuple<std::set<char>, std::tuple<int,int>>>>> q;
    q.emplace(sx,sy,0,std::set<char>(), std::set<std::tuple<std::set<char>, std::tuple<int,int>>>());
/*
    auto buildreachable = [](int x, int y, const std::set<char> & keys, const std::vector<std::string<){

    };
  */
    while(!q.empty()) {
        auto [x,y,step,keys,visited] = q.front(); q.pop();
        if(map[y][x] == '#') continue;
        if(visited.find(std::tuple(keys, std::tuple(x,y))) != visited.end()) continue;
        visited.insert(std::tuple(keys, std::tuple(x,y)));
        if(islower(map[y][x])) keys.insert(map[y][x]);
        if(keys.size() == nkeys) {
            ans1 = step;
            break;
        }
        if(isupper(map[y][x]) && keys.find(tolower(map[y][x])) == keys.end()) continue;
        q.emplace(x+1, y, step+1, keys, visited);
        q.emplace(x-1, y, step+1, keys, visited);
        q.emplace(x, y-1, step+1, keys, visited);
        q.emplace(x, y+1, step+1, keys, visited);
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p18(std::cin);
}
