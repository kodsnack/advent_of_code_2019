#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <unordered_map>

void p06(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;

    std::unordered_map<std::string, std::string> map;

    {
        bool done = false;
        std::string str, f, s;

        while (!done) {
            char c;
            is.get(c);
            if (!is.good()) {
                done = true;
                c = '\n';
            }

            if((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
                str.push_back(c);
            } else {
                if(!str.empty()) {
                    if(f.empty()) f = str;
                    else s = str;
                }

                if(c == '\n') {
                    if(!f.empty() && !s.empty()) map[s] = f;
                    f.clear();
                    s.clear();
                }
                str.clear();
            }

        }
    }

    std::unordered_map<std::string, std::vector<std::string>> paths;
    for(const auto & [f,tt] : map) {
        auto t = tt;
        while(true) {
            paths[f].push_back(t);
            auto it = map.find(t);
            if(it != map.end()) {
                t = it->second;
            } else break;
        }
    }

    for(const auto & [k,v] : paths) ans1 += v.size();

    auto & v1 = paths["YOU"];
    auto & v2 = paths["SAN"];
    for(size_t i1 = 0; i1 < v1.size(); i1++) {
        for(size_t i2 = 0; i2 < v2.size(); i2++) {
            if(v1[i1] == v2[i2]){
                ans2 = i1+i2;
                break;
            }
        }
        if(ans2) break;
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p06(std::cin);
}
