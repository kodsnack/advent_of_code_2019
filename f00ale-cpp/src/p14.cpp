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
    const auto m = [&is]
    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        std::string str;
        std::vector<R> v;
        int saved_num = 0;
        std::map<R, std::vector<R>> m;

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
        return m;
    }();

    constexpr int64_t trillion = 1000000000000;
    int64_t min = 0;
    int64_t max = 10000000;
    ans2 = 1; // check 1 first
    do {
        std::queue<R> q;
        std::map<std::string, int64_t> rest;
        q.emplace("FUEL", ans2);
        int64_t nore = 0;
        while (!q.empty()) {
            auto[needs, needn] = q.front();
            q.pop();
            for (auto &[rk, rv] : m) {
                auto &[rs, rn] = rk;
                if (rs == needs) {
                    if (auto & tmp = rest[needs]; tmp <= needn) {
                        needn -= tmp;
                        tmp = 0;
                    } else {
                        tmp -= needn;
                        needn = 0;
                        continue;
                    }
                    int64_t mult = needn / rn;
                    if (needn % rn) mult++;
                    rest[needs] += mult * rn - needn;
                    for (auto &[sx, nx] : rv) {
                        auto ant = (mult * nx);

                        if (sx == "ORE") {
                            nore += ant;
                        } else {
                            q.emplace(sx, ant);
                        }
                    }
                }
            }
        }

        if(ans2 == 1) ans1 = nore;
        if(nore > trillion) max = ans2;
        else min = ans2;
        ans2 = (max + min) / 2;
    } while(max-min > 1);

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p14(std::cin);
}
