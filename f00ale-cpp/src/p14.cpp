#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <map>
#include <queue>

void p14(std::istream & is) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    using R = std::tuple<std::string, int64_t>;
    std::map<R, std::vector<R>> m;
    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        std::string str;
        std::vector<R> v;
        int saved_num = 0;
        while (!done) {
            char c;
            is.get(c);
            if (!is.good()) {
                done = true;
                c = '\n';
            }

            if(c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
                have_num = true;
            } else if((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
                str.push_back(c);
            } else {
                if(have_num) {
                    saved_num = num;
                } else if(!str.empty()) {
                    v.emplace_back(str,saved_num);
                }

                if(c == '\n') {
                    if(!v.empty()) {
                        auto s = v.back();
                        v.pop_back();
                        m[s] = v;
                    }
                    v.clear();
                }
                have_num = false;
                num = 0;
                str.clear();
            }

        }
    }
/*
    for(auto & [k,v] : m) {
        auto & [s,n] = k;
        std::cout << n << " " << s << " <- ";
        for(auto & [sx, nx] : v) {
            std::cout << nx << " " << sx << ", ";
        }
        std::cout <<std::endl;
    }
*/
    bool found = false;

    std::map<std::string, int> needed;
    needed["FUEL"] = 1;
    std::queue<R> q;
    /*
    while(needed.find("ORE") == needed.end()) {
        for (auto &[k, v] : m) {
            auto & [s, ned] = k;
            auto it = needed.find(s);
            if(it != needed.end()) {
                auto nn = it->second;
                needed.erase(it);
                std::cout << s << " " << nn << " : ";
                for(auto & [sx, nx] : v) {
                    needed[sx] += nn*nx/ned;
                    std::cout << " " << sx << " " << nn*nx/ned <<  ", ";
                }
                std::cout << std::endl;
            }
        }
    }
*/
    std::map<std::string, int64_t> rest;

    q.emplace("FUEL",3445249);
    while(!q.empty()) {
        auto[needs, needn] = q.front();
        q.pop();
        for (auto &[rk, rv] : m) {
            auto & [rs ,rn] = rk;
            if(rs == needs) {
                auto tmp = rest[needs];
                //std::cout << needn << " " << needs << "(have " << tmp << ") <- ";
                if(tmp <= needn) { needn-=tmp; rest[needs] = 0; }
                else {
                    rest[needs] -= needn;
                    needn = 0;
                    //std::cout << "-------" << std::endl;
                    continue;
                }
                int64_t mult = 0;
                mult = needn / rn;
                if(needn % rn) mult++;
                rest[needs] += mult*rn - needn;
                for(auto & [sx, nx] : rv) {
                    auto ant = (mult*nx);
                    //std::cout << ant << " " << sx << " (" << needn << "," << nx << "," << rn << ") ";
                    if(sx == "ORE") {
                        ans1 += ant;
                    } else {
                        q.emplace(sx, ant);
                    }
                }
                //std::cout << std::endl;
            }
        }
    }
    std::cout << "1000000000000" << std::endl;
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p14(std::cin);
}
