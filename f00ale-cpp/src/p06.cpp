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
        int num = 0;
        bool have_num = false;
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

    for(auto & p : map) {
        auto [a,b] = p;
        auto it = map.find(b);
        std::cout << a << " -> " << b;
        ans1++;
        while(it != map.end()) {
            std::cout << " -> " << it->second;
            ans1++;
            it = map.find(it->second);
        }
        std::cout << std::endl;
    }

    std::unordered_map<std::string, std::vector<std::string>> xxx;
    for(auto q : {"YOU", "SAN"}) {
        auto it = map.find(q);
        std::cout << q;
        while(it != map.end()) {
            std::cout << " -> " << it->second;
            xxx[q].push_back(it->second);
            it = map.find(it->second);
        }
        std::cout << std::endl;
    }
    auto & v1 = xxx["YOU"];
    auto & v2 = xxx["SAN"];
    for(int i1 = 0; i1 < v1.size(); i1++) {
        for(int i2 = 0; i2 < v2.size(); i2++) {
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
