#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <set>

void p24(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;

    const std::vector<std::string> cmap = [&is]{
        std::vector<std::string> ret;
        bool done = false;
        bool neednl = true;
        while (!done) {
            char c;
            is.get(c);
            if (!is.good()) {
                done = true;
                c = '\n';
            }

            if(c == '\n') {
                neednl = true;
            } else {
                if(neednl) {
                    ret.push_back({});
                    neednl = false;
                }
                ret.back().push_back(c);
            }
        }
        return ret;
    }();

    auto countbugs = [](const std::vector<std::string> & m, int x, int y) {
        int cnt = 0;
        if (y > 0 && m[y - 1][x] == '#') cnt++;
        if (y < 5 - 1 && m[y + 1][x] == '#') cnt++;
        if (x > 0 && m[y][x - 1] == '#') cnt++;
        if (x < 5 - 1 && m[y][x + 1] == '#') cnt++;
        return cnt;
    };

    { // part 1
        std::set<std::vector<std::string>> seen;
        auto map = cmap;
        bool done = false;
        while (!done) {
            seen.emplace(map);
            std::vector<std::string> next(5);
            for (auto &s : next) s.resize(5);
            for (int y = 0; y < 5; y++) {
                for (int x = 0; x < 5; x++) {
                    int cnt = countbugs(map, x, y);

                    if (map[y][x] == '#') {
                        if (cnt == 1) next[y][x] = '#';
                        else next[y][x] = '.';
                    } else {
                        if (cnt == 1 || cnt == 2) next[y][x] = '#';
                        else next[y][x] = '.';
                    }
                }
            }
            map.swap(next);
            if (seen.find(map) != seen.end()) done = true;
        }

        for (int i = 0; i < 25; i++) {
            if (map[i / 5][i % 5] == '#') {
                ans1 += (1u << i);
            }
        }
    }

    std::vector<std::vector<std::string>> maps;
    maps.push_back(cmap);
    maps.front()[2][2] = '?';
    for(int iter = 0; iter < 200; iter++) {
        std::vector<std::vector<std::string>> nextmaps;

        // first add outer if needed
        {
            std::vector<std::string> next(5);
            for (auto &s : next) s.resize(5, '.');
            auto & map = maps.front();
            // if 1 or two at outer rim of old top
            int uc = 0, dc = 0;
            for(int x = 0; x < 5; x++) {
                if(map[0][x] == '#') uc++;
                if(map[5-1][x] == '#') dc++;
            }
            int lc = 0, rc = 0;
            for(int y = 0; y < 5; y++) {
                if(map[y][0] == '#') lc++;
                if(map[y][5-1] == '#') rc++;
            }
            bool add = false;
            if(uc == 1 || uc == 2) { add = true; next[2-1][2] = '#'; }
            if(dc == 1 || dc == 2) { add = true; next[2+1][2] = '#'; }
            if(lc == 1 || lc == 2) { add = true; next[2][2-1] = '#'; }
            if(rc == 1 || rc == 2) { add = true; next[2][2+1] = '#'; }
            if(add) {
                next[2][2] = '?';
                nextmaps.push_back(next);
            }
        }

        auto countbugs2 = [&maps, countbugs](size_t level, int x, int y) {
            // current level
            int cnt = countbugs(maps[level], x, y);

            if(level > 0) { // outer level exists
                if (x == 0) if(maps[level-1][2][2-1] == '#') cnt++;
                if (x == 5-1) if(maps[level-1][2][2+1] == '#') cnt++;
                if (y == 0) if(maps[level-1][2-1][2] == '#') cnt++;
                if (y == 5-1) if(maps[level-1][2+1][2] == '#') cnt++;
            }
            if(level+1 < maps.size()) { // inner level exists
                if (x == 2 && y == 1) for(int ix = 0; ix < 5; ix++) if(maps[level+1][0][ix] == '#') cnt++;
                if (x == 2 && y == 3) for(int ix = 0; ix < 5; ix++) if(maps[level+1][5-1][ix] == '#') cnt++;
                if (x == 1 && y == 2) for(int iy = 0; iy < 5; iy++) if(maps[level+1][iy][0] == '#') cnt++;
                if (x == 3 && y == 2) for(int iy = 0; iy < 5; iy++) if(maps[level+1][iy][5-1] == '#') cnt++;
            }
            return cnt;
        };

        for (size_t level = 0; level < maps.size(); level++) {
            const auto & cmap = maps[level];
            std::vector<std::string> next(5);
            for (auto &s : next) s.resize(5);
            for (int y = 0; y < 5; y++) {
                for (int x = 0; x < 5; x++) {
                    if(x==2&&y==2) { next[y][x]='?';continue;}

                    int cnt = countbugs2(level, x, y);
                    if (cmap[y][x] == '#') {
                        if (cnt == 1) next[y][x] = '#';
                        else next[y][x] = '.';
                    } else {
                        if (cnt == 1 || cnt == 2) next[y][x] = '#';
                        else next[y][x] = '.';
                    }
                }
            }
            nextmaps.push_back(next);
        }

        // create inner if needed
        {
            std::vector<std::string> next(5);
            for (auto &s : next) s.resize(5, '.');
            auto & m = maps.back();
            bool add = false;
            if(m[2][1] == '#') { add = true; for(int y = 0; y < 5; y++) next[y][0] = '#'; }
            if(m[2][3] == '#') { add = true; for(int y = 0; y < 5; y++) next[y][5-1] = '#'; }
            if(m[1][2] == '#') { add = true; for(int y = 0; y < 5; y++) next[0][y] = '#'; }
            if(m[3][2] == '#') { add = true; for(int y = 0; y < 5; y++) next[5-1][y] = '#'; }
            if(add) {
                next[2][2] = '?';
                nextmaps.push_back(next);
            }
        }
        maps.swap(nextmaps);
    }

    for(auto & map : maps) {
        for(auto & s : map) {
            for(auto c : s) if(c=='#') ans2++;
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p24(std::cin);
}
