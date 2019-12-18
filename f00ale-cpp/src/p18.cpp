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
    std::vector<std::tuple<char,int,int>> keys;

    for(auto & s : map) {
        int px = 0;
        for(auto c : s) {
            if(c=='@') {
                sx = px;
                sy = i;
            }
            if(islower(c)) {
                keys.emplace_back(c, px, i);
            }
            px++;
        }
        i++;
    }

    nkeys = keys.size();
    std::vector<std::tuple<int,int>> keypos(keys.size());
    for (auto & k : keys) {
        char c; int x, y;
        std::tie(c,x,y) = k;
        keypos[c-'a'] = std::make_tuple(x,y);
    }

    auto buildreachable = [](int sx, int sy, const std::vector<std::string> & map){
        std::vector<std::tuple<char, int, uint32_t>> ret;
        std::queue<std::tuple<int,int,int,uint32_t>> q;
        q.emplace(std::make_tuple(sx,sy,0,0));
        std::set<std::tuple<int,int>> visited;
        while(!q.empty()) {
            auto & tup = q.front();
            auto x = std::get<0>(tup);
            auto y = std::get<1>(tup);
            auto step = std::get<2>(tup);
            auto neededkeys = std::get<3>(tup);
            q.pop();
            char c = map[y][x];
            if(c == '#') continue;
            if(visited.find(std::make_tuple(x,y)) != visited.end()) continue;
            visited.insert(std::make_tuple(x,y));
            if(islower(c)) {
                ret.emplace_back(c, step, neededkeys);
            }
            //if(isupper(c) && !(keys & (1<<(tolower(c)-'a')))) continue;
            if(isupper(c)) {
                neededkeys |= (1<<(tolower(c)-'a'));
            }
            q.emplace(x+1, y, step+1, neededkeys);
            q.emplace(x-1, y, step+1, neededkeys);
            q.emplace(x, y-1, step+1, neededkeys);
            q.emplace(x, y+1, step+1, neededkeys);
        }
        return ret;
    };

    constexpr int nrobots = 4;

    std::vector<std::vector<std::tuple<uint32_t, int>>> k2k(keys.size());
    for(auto & v : k2k) v.resize(keys.size());//,std::make_tuple(0u,-1));

    std::vector<std::tuple<int,int>> robots;
    if(nrobots > 1) {
        for(int dy = -1; dy <= 1; dy++) {
            for(int dx = -1; dx <= 1; dx++) {
                map[sy+dy][sx+dx] = (dx&&dy)?'@':'#';
                if(dy&&dx) robots.emplace_back(sx+dx,sy+dy);
            }
        }
    } else {
        robots.emplace_back(sx,sy);
    }

    for(auto [x,y] : robots) {
        auto v = buildreachable(x,y,map);
        k2k.emplace_back(keys.size());//, std::make_tuple(0u,-1));
        for(auto & t : v) {
            uint32_t needed;
            char key;
            int steps;
            std::tie(key, steps, needed) = t;
            k2k.back()[key-'a'] = std::make_tuple(needed, steps);
        }
    }

    for(auto [c,x,y] : keys) {
        auto v = buildreachable(x,y, map);
        for(auto & t : v) {
            uint32_t needed;
            char key;
            int steps;
            std::tie(key, steps, needed) = t;
            k2k[c-'a'][key-'a'] = std::make_tuple(needed, steps);
        }
    }

    uint32_t allmask = 0;
    for(int q = 0; q < nkeys; q++) {
        allmask <<= 1u;
        allmask |= 1u;
    }

    ans1 = std::numeric_limits<int>::max();

    std::map<std::tuple<uint32_t, std::array<char,nrobots>>, int> memory;
    std::array<char,nrobots> init;
    for(int r = 0; r < nrobots; r++) {
        init[r] = 'a'+keys.size()+r;
    }
    memory.emplace(std::make_tuple(0, init), 0);
    for(int i = 0; i < nkeys; i++) {
        std::map<std::tuple<uint32_t, std::array<char,nrobots>>, int> new_memory;
        for(const auto & p : memory) {
            auto & tup = p.first;
            auto keys = std::get<0>(tup);
            auto lasta = std::get<1>(tup);
            auto stepshere = p.second;

            // loop over accessible
            for(int r = 0; r < nrobots; r++) {
                auto last = lasta[r];
                for (size_t i = 0; i < k2k[last - 'a'].size(); i++) {
                    if (i == last - 'a') continue;
                    auto &in = k2k[last - 'a'][i];
                    auto needed = std::get<0>(in);
                    auto steps = std::get<1>(in);
                    if(!steps) continue;
                    if ((keys & needed) == needed) {
                        // have all needed keys
                        char key = 'a' + i;
                        auto new_keys = keys | (1 << i);
                        if (new_keys == keys) continue;
                        auto lastacpy = lasta;
                        lastacpy[r] = key;
                        auto &d = new_memory[std::make_tuple(new_keys, lastacpy)];
                        if (!d || d > stepshere + steps) {
                            d = stepshere + steps;
                        }
                    }
                }
            }
        }
        memory.swap(new_memory);
    }

    for(auto & m : memory) {
        if (m.second < ans1) ans1 = m.second;
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

#include <fstream>

int main() {
    p18(std::cin); exit(0);
    std::ifstream in("data/p18t0.txt");
    p18(in);
}
