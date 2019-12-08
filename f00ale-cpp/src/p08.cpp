#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <set>

void p08(std::istream & is) {
    int ans1 = 0;

    constexpr int W = 25;
    constexpr int H = 6;
    constexpr int S = W*H;
    std::vector<std::string> lays;
    {
        bool done = false;
        std::string str;

        while (!done) {
            char c;
            is.get(c);
            if (!is.good()) {
                done = true;
                c = '\n';
            }

            if(c >= '0' && c <= '9') {
                str.push_back(c);
                if(str.size() == S) {
                    lays.push_back(str);
                    str.clear();
                }
            }

        }
    }
    std::string ans2(S, '2');

    int min_zeroes = std::numeric_limits<int>::max();
    for(auto & l : lays) {
        int zeroes = 0, ones = 0, twos = 0;
        for(int i = 0; i < S; i++) {
            auto c = l[i];
            switch(c) {
                case '0': zeroes++; if(ans2[i] == '2') ans2[i] = ' '; break;
                case '1': ones++; if(ans2[i] == '2') ans2[i] = '*'; break;
                case '2': twos++; break;
            }
        }
        if(zeroes < min_zeroes) {
            min_zeroes = zeroes;
            ans1 = ones * twos;
        }
    }

    std::cout << ans1 << std::endl;

    for(int y = 0; y < H; y++) {
        std::cout << ans2.substr(y*W, W) << std::endl;
    }
}

int main() {
    p08(std::cin);
}
