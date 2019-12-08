#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <set>

void p08(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    constexpr int W = 25;
    constexpr int H = 6;
    constexpr int S = W*H;
    std::vector<std::string> lays;
    {
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
                if(str.size() == 25*6) {
                    lays.push_back(str);
                    str.clear();
                }
                str.push_back(c);
            } else {
                if(str.size() == 25*6) {
                    lays.push_back(str);
                    str.clear();
                }
            }

        }
    }
    std::string out;
    for(int i = 0; i <6*25; i++) out.push_back('2');
    int zeroes = std::numeric_limits<int>::max();
    for(auto & l : lays) {
        int z = 0, o = 0, t = 0;
        for(int i = 0; i < 6*25; i++) {
            auto c = l[i];
            switch(c) {
                case '0': z++; if(out[i]=='2') out[i] = ' '; break;
                case '1': o++; if(out[i]=='2') out[i] = 'x'; break;
                case '2': t++; break;
            }
        }
        if(z < zeroes) {
            zeroes=z;
            ans1 = o*t;
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
    for(int y = 0; y < 6; y++) {
        for(int x = 0; x < 25; x++) {
            std::cout << out[y*25+x];
        }
        std::cout << std::endl;
    }
}
// 2193 fel
int main() {
    p08(std::cin);
}
