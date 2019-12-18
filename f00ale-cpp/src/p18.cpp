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

    int sx = 0, sy = 0;
    std::vector<std::tuple<char,int,int>> keys;
    {
        int row = 0;
        for (auto &s : map) {
            int col = 0;
            for (auto c : s) {
                if (c == '@') {
                    sx = col;
                    sy = row;
                }
                if (islower(c)) {
                    keys.emplace_back(c, col, row);
                }
                col++;
            }
            row++;
        }
    }
    int nkeys = keys.size();

    auto buildreachable = [](int sx, int sy, const std::vector<std::string> & map){
        std::vector<std::tuple<char, int, uint32_t>> ret;
        std::queue<std::tuple<int,int,int,uint32_t>> q;
        q.emplace(std::make_tuple(sx,sy,0,0));
        std::set<std::tuple<int,int>> visited;
        while(!q.empty()) {
            auto [x,y,step,neededkeys] = q.front();
            q.pop();
            char c = map[y][x];
            if(c == '#') continue;
            if(visited.find(std::make_tuple(x,y)) != visited.end()) continue;
            visited.insert(std::make_tuple(x,y));
            if(islower(c)) {
                ret.emplace_back(c, step, neededkeys);
            }
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

    for(auto problem : {1,2}) {
        int nrobots = problem == 1 ? 1 : 4;

        std::vector<std::vector<std::tuple<uint32_t, int>>> k2k(keys.size());
        for (auto &v : k2k) v.resize(keys.size());

        std::vector<std::tuple<int, int>> robots;
        if (nrobots > 1) {
            for (int dy = -1; dy <= 1; dy++) {
                for (int dx = -1; dx <= 1; dx++) {
                    map[sy + dy][sx + dx] = (dx && dy) ? '@' : '#';
                    if (dy && dx) robots.emplace_back(sx + dx, sy + dy);
                }
            }
        } else {
            robots.emplace_back(sx, sy);
        }

        for (auto[x, y] : robots) {
            auto v = buildreachable(x, y, map);
            k2k.emplace_back(keys.size());
            for (auto [key, steps, needed] : v) {
                k2k.back()[key - 'a'] = std::make_tuple(needed, steps);
            }
        }

        for (auto[c, x, y] : keys) {
            auto v = buildreachable(x, y, map);
            for (auto [key, steps, needed] : v) {
                k2k[c - 'a'][key - 'a'] = std::make_tuple(needed, steps);
            }
        }

        std::map<std::tuple<uint32_t, std::array<char, 4>>, int> memory;

        std::array<char, 4> init{};
        for (int r = 0; r < nrobots; r++) {
            init[r] = 'a' + keys.size() + r;
        }
        memory.emplace(std::make_tuple(0, init), 0);

        for (int q = 0; q < nkeys; q++) {
            std::map<std::tuple<uint32_t, std::array<char, 4>>, int> new_memory;
            for (const auto &[tup, stepshere] : memory) {
                auto & [keys, lasta] = tup;

                for (int r = 0; r < nrobots; r++) {
                    auto last = lasta[r];

                    int i = 0;
                    for (auto [needed, steps] : k2k[last - 'a']) {
                        char key = 'a' + i;
                        auto new_keys = keys | (1u << i);
                        i++;
                        if (!steps) continue;
                        if ((keys & needed) == needed) {
                            // have all needed keys
                            if (new_keys == keys) continue; // key isn't new
                            auto newlasta = lasta;
                            newlasta[r] = key;

                            auto &d = new_memory[std::make_tuple(new_keys, newlasta)];
                            if (!d || d > stepshere + steps) {
                                d = stepshere + steps;
                            }
                        }
                    }
                }
            }
            memory.swap(new_memory);
        }
        int ans = std::numeric_limits<int>::max();
        for (auto &[_,stepshere] : memory) {
            if (stepshere < ans) ans = stepshere;
        }

        if(problem == 1) {
            ans1 = ans;
        } else {
            ans2 = ans;
        }
    }
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

#include <fstream>

int main() {
    p18(std::cin);
}
