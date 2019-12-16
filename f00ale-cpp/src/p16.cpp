#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <set>
#include <sstream>

void p16(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    const std::vector<int> input = [&is]
    {
        std::vector<int> v;
        bool done = false;
        int num = 0;
        bool have_num = false;
        std::string str;

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

                v.push_back(num);
                num = 0;
            } else if((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
                str.push_back(c);
            } else {
                if(have_num) {

                } else if(!str.empty()) {

                }

                if(c == '\n') {

                }
                have_num = false;
                num = 0;
                str.clear();
            }

        }
        return v;
    }();

    for(auto problem : {1,2}) {
        size_t offset = 0;
        auto v = input;
        v.reserve(input.size() * 10000);
        if(problem == 2) {
            for (int i = 0; i < 7; i++) {
                offset *= 10;
                offset += v[i];
            }
            auto cpy = v;
            for (int i = 1; i < 10000; i++) v.insert(v.end(), cpy.begin(), cpy.end());
        }

        const std::vector<int> phase{0, 1, 0, -1};
        std::vector<int> precalc(v.size()+1);

        for (int varv = 0; varv < 100; varv++) {
            for (size_t i = offset; i < v.size(); i++) {
                precalc[i+1] = precalc[i]+v[i];
            }

            for(size_t i = offset; i < v.size()/2; i++) {
                int pp = 0;
                int tmp = 0;
                size_t stride = i;
                size_t nextstride = i;
                for (size_t j = 0; j < v.size(); j += stride) {
                    stride = nextstride;
                    tmp += phase[pp]*(precalc[std::min(static_cast<size_t>(j+stride),v.size())]-precalc[j]);
                    nextstride = i+1;
                    pp++;
                    if(pp > 3) pp = 0;
                }
                v[i] = (std::abs(tmp) % 10);
            }
            for(size_t i = std::max(offset, v.size()/2); i < v.size(); i++) {
                v[i] = std::abs(precalc[v.size()]-precalc[i]) % 10; //phase is always 1
            }
        }
        int ans = 0;
        for(size_t i = offset; i < offset+8; i++) {
            ans *= 10;
            ans += v[i];
        }
        if(problem == 1) ans1 = ans;
        else ans2 = ans;
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p16(std::cin);
}
