#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <set>

void p04(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    int inmin = 0;
    int inmax = 0;

    {
        bool done = false;
        int num = 0;
        bool have_num = false;

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
            } else {
                if(have_num) {
                    if(inmin) inmax = num;
                    else inmin = num;
                }
                have_num = false;
                num = 0;
            }

        }
    }

    for(int i = inmin; i <= inmax; i++) {
        int last = 11;
        int len = 0;
        bool a1 = false, a2 = false;

        auto tmp = i;
        while(tmp) {
            auto c = tmp % 10;
            tmp = tmp / 10;
            if(c > last) { a1 = a2 = false; len = 0; break; }
            if(c == last) len++;
            else{
                if(len >= 2) a1 = true;
                if(len == 2) a2 = true;
                len = 1;
                last = c;
            }
        }
        if(a1 || len >= 2) ans1++;
        if(a2 || len == 2) ans2++;
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p04(std::cin);
}
