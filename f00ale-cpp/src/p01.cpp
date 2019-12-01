#include <iostream>

void p01(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;

    {
        bool done = false;
        int num = 0;

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
            } else {
                if(num) {
                    num = num / 3 - 2;
                    ans1 += num;
                    while(num > 0) {
                        ans2 += num;
                        num = num / 3 - 2;
                    }
                }
                num = 0;
            }

        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p01(std::cin);
}
