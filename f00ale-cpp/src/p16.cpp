#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <set>

void pXX(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    std::vector<int> v;
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
    }

    std::vector<int> phase{0,1,0,-1};

    for(int varv = 0; varv < 100; varv++) {
        std::vector<int> next;
        for(int i = 0; i < v.size(); i++) {
            auto pp = 0;
            auto rep = i+1;
            auto tmp = 0;
            for(auto e : v) {
                rep--;
                if(rep == 0) {
                    pp = (pp+1) % phase.size();
                    rep = i+1;
                }
                //std::cout << e << "*" << phase[pp] << " ";
                tmp += phase[pp]*e;
            }
            next.push_back(std::abs(tmp) % 10);
            //std::cout << "= " << next.back() << std::endl;
            std::cout << next.back();
        }
        std::cout << std::endl;
        v.swap(next);
    }

    for(int i = 0; i < 8; i++) std::cout << v[i] ;
    std::cout << std::endl;

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
// 5329608276 fel
int main() {
    pXX(std::cin);
}
