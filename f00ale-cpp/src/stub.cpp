#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <set>

void pXX(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;

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



    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    pXX(std::cin);
}
