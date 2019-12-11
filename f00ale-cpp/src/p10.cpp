#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <set>
#include <sstream>
#include <numeric>
#include <cmath>

struct pos {
    pos() = default;
    pos(const pos &) = default;
    pos(double at, double ad, int ar, int ac) :
            t(at), d(ad), r(ar), c(ac) {}
    double t, d;
    int r,c;
};

bool operator<(const pos & p1, const pos & p2) {
    if(p1.t==p2.t) return p1.d < p2.d;
    else return p1.t < p2.t;
}

void p10(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    std::vector<std::vector<char>> v;
    {
        bool done = false;
        bool need_new = true;
        while (!done) {
            char c;
            is.get(c);
            if (!is.good()) {
                done = true;
                c = '\n';
            }

            if(c == '#' || c == '.') {
                if(need_new) {
                    v.emplace_back();
                }
                need_new = false;
                v.back().push_back(c);
            } else {
                need_new = true;
            }
        }
    }

    int max = 0, maxr = 0, maxc = 0;

    for(int row = 0; row < static_cast<int>(v.size()); row++) {
        for(int col = 0; col < static_cast<int>(v[row].size()); col++) {
            if(v[row][col] != '#') continue;

            std::vector<pos> angs;
            for(int r = 0; r < static_cast<int>(v.size()); r++) {
                for(int c = 0; c < static_cast<int>(v[row].size()); c++) {
                    if(row==r && col == c) continue;
                    if(v[r][c] != '#') continue;

                    auto tmp = atan2(c-col, row-r);

                    if(tmp < 0) tmp+=2*M_PI;
                    angs.emplace_back(tmp, sqrt(r*r+c*c), r, c);
                }
            }

            std::sort(angs.begin(), angs.end());
            int ant = 0;
            auto last = -2*M_PI;
            for(auto d : angs) {
                if(fabs(last-d.t) > .0001) {
                    ant++;
                }
                last = d.t;
            }

            if(ant > max) {
                max = ant;
                maxr = row;
                maxc = col;

            }
        }
    }

    ans1 = max;

    int zapp = 0;

    while(zapp < 200) {
        auto nextv = v;
        int row = maxr;
        int col = maxc;
        std::vector<pos> angs;
        for(int r = 0; r < static_cast<int>(v.size()); r++) {
            for(int c = 0; c < static_cast<int>(v[row].size()); c++) {
                if(row==r && col == c) continue;
                if(v[r][c] != '#') continue;

                auto tmp = atan2(c-col, row-r);

                if(tmp < 0) tmp+=2*M_PI;
                angs.emplace_back(tmp, sqrt(r*r+c*c), r, c);
            }
        }

        std::sort(angs.begin(), angs.end());

        auto last = -2*M_PI;
        if(!angs.size()) {
            break;
        }
        for(auto d : angs) {
            if(fabs(last-d.t) > .0001) {
                zapp++;
                nextv[d.r][d.c] = 'x';
                if(zapp == 200) {
                    ans2=d.c*100+d.r;
                }
            }
            last = d.t;
        }
        v=nextv;
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p10(std::cin);
}
