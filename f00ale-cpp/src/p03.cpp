#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <map>
#include <unordered_map>

void p03(std::istream & is) {
    int ans1 = std::numeric_limits<int>::max();
    int ans2 = std::numeric_limits<int>::max();

    std::map<std::tuple<int,int>, int> steps;

    {
        bool done = false;
        int num = 0;
        bool have_num = false;

        int varv = 1;
        int x = 0, y = 0;
        char d = 0;
        int step = 0;

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
                d = c;
            } else {
                if(have_num && d) {
                    while(num--) {
                        step++;
                        switch (d) {
                            case 'U': y++; break;
                            case 'D': y--; break;
                            case 'R': x++; break;
                            case 'L': x--; break;
                        }
                        if(varv == 1)
                            steps[std::make_tuple(x,y)] = step;
                        else {
                            if(auto s = steps[std::make_tuple(x,y)]) {
                                if(s + step < ans2) ans2 = s + step;
                                if(std::abs(x) + std::abs(y) < ans1) ans1 = std::abs(x)+std::abs(y);
                            }
                        }
                    }
                }

                if(c == '\n') {
                    varv++;
                    x = y = 0;
                    step = 0;
                }
                d = 0;
                have_num = false;
                num = 0;
            }
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p03(std::cin);
}
