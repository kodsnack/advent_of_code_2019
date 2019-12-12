#include <iostream>
#include <vector>
#include <numeric>
#include <tuple>
#include <map>

void p12(std::istream & is) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<std::tuple<int64_t,int64_t,int64_t,int64_t,int64_t,int64_t>> state;

    {
        std::vector<int64_t> tmp;
        bool done = false;
        int num = 0;
        bool have_num = false;

        bool neg = false;
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
            } else if (c == '-' && !have_num) {
                neg = true;
            } else {
                if(have_num) {
                    num *= neg ? -1 : 1;
                    tmp.push_back(num);
                }
                if(c == '\n') {
                    if(tmp.size() == 3) {
                        state.emplace_back(tmp[0], 0, tmp[1], 0, tmp[2], 0);
                    }
                    tmp.clear();
                }
                have_num = false;
                num = 0;
                neg = false;
            }

        }
    }

    int64_t iter = 0;

    const auto state0 = state;

    int64_t xcyc = 0, ycyc = 0, zcyc = 0;

    while(iter < 1000 || !xcyc || !ycyc || !zcyc) {
        iter++;
        for(size_t a = 0; a < state.size(); a++) {
            auto & [x1,dx1,y1,dy1,z1,dz1] = state[a];
            for (size_t b = a + 1; b < state.size(); b++) {
                auto & [x2,dx2,y2,dy2,z2,dz2] = state[b];

                if(x1 < x2) { dx1++; dx2--; }
                else if(x1 > x2) { dx1--; dx2++; }
                if(y1 < y2) { dy1++; dy2--; }
                else if(y1 > y2) { dy1--; dy2++; }
                if(z1 < z2) { dz1++; dz2--; }
                else if(z1 > z2) { dz1--; dz2++; }

            }
        }

        for(auto & [x,dx,y,dy,z,dz] : state) {
            x+=dx;
            y+=dy;
            z+=dz;
        }

        bool xc = true, yc = true, zc = true;
        for(size_t i = 0; i < state.size(); i++) {
            auto [x,dx,y,dy,z,dz] = state[i];
            auto [x0,dx0,y0,dy0,z0,dz0] = state0[i];
            if(x != x0 || dx != dx0) xc = false;
            if(y != y0 || dy != dy0) yc = false;
            if(z != z0 || dz != dz0) zc = false;
        }

        if(!xcyc && xc) xcyc = iter;
        if(!ycyc && yc) ycyc = iter;
        if(!zcyc && zc) zcyc = iter;

        if(iter == 1000) {
            for(auto [x,dx,y,dy,z,dz] : state) {
                ans1 += (std::abs(x)+std::abs(y)+std::abs(z))*(std::abs(dx)+std::abs(dy)+std::abs(dz));
            }
        }
    }

    ans2 = std::lcm(std::lcm(xcyc, ycyc), zcyc);

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p12(std::cin);
}
