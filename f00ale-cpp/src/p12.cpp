#include <iostream>
#include <vector>
#include <numeric>
#include <tuple>
#include <map>

void p12(std::istream & is) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    std::vector<std::tuple<int64_t,int64_t,int64_t>> pos;
    std::vector<std::tuple<int64_t,int64_t,int64_t>> vel;
    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        std::string str;
        int x,y,z;
        bool hx = false, hy = false, hz = false;
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
                    if(!hx) {
                        x = num * (neg?-1:1);
                        hx = true;
                    } else if(!hy) {
                        y = num * (neg?-1:1);
                        hy = true;
                    } else {
                        z = num * (neg?-1:1);
                        hz = true;
                    }
                } else if(!str.empty()) {

                }
                if(c == '\n') {
                    if(hz){
                        pos.emplace_back(x, y, z);
                        x = y = z = 0;
                        hx = hy = hz = false;
                    }
                }
                have_num = false;
                num = 0;
                neg = false;
                str.clear();
            }

        }
    }
    vel.resize(pos.size(), std::tuple(0,0,0));

    int64_t iter = 0;

    auto pos0 = pos;
    auto vel0 = vel;
    std::map<std::tuple<std::tuple<int64_t,int64_t,int64_t>,std::tuple<int64_t,int64_t,int64_t>>,int> history[4];

    std::map<std::tuple<int64_t,int64_t,int64_t,int64_t,int64_t,int64_t,int64_t,int64_t>, int> xhist;
    std::map<std::tuple<int64_t,int64_t,int64_t,int64_t,int64_t,int64_t,int64_t,int64_t>, int> yhist;
    std::map<std::tuple<int64_t,int64_t,int64_t,int64_t,int64_t,int64_t,int64_t,int64_t>, int> zhist;
    int64_t xcyc = 0, ycyc = 0, zcyc = 0;
    bool fx  = false, fy = false, fz = false;
    while(true) {
        iter++;
        for(int a = 0; a < pos.size(); a++) {
            auto [x1,y1,z1] = pos[a];
            auto & [dx1,dy1,dz1] = vel[a];
            for (int b = a + 1; b < pos.size(); b++) {
                auto [x2,y2,z2] = pos[b];
                auto & [dx2,dy2,dz2] = vel[b];

                if(x1 < x2) { dx1++; dx2--; }
                else if(x1 > x2) { dx1--; dx2++; }
                if(y1 < y2) { dy1++; dy2--; }
                else if(y1 > y2) { dy1--; dy2++; }
                if(z1 < z2) { dz1++; dz2--; }
                else if(z1 > z2) { dz1--; dz2++; }

            }
        }

        for(int a = 0; a < pos.size(); a++) {
            auto&[x, y, z] = pos[a];
            auto[dx, dy, dz] = vel[a];
            x+=dx;
            y+=dy;
            z+=dz;
        }

        auto xs = std::tuple(std::get<0>(pos[0]), std::get<0>(vel[0]),std::get<0>(pos[1]), std::get<0>(vel[1]),std::get<0>(pos[2]), std::get<0>(vel[2]),std::get<0>(pos[3]), std::get<0>(vel[3]));
        auto ys = std::tuple(std::get<1>(pos[0]), std::get<1>(vel[0]),std::get<1>(pos[1]), std::get<1>(vel[1]),std::get<1>(pos[2]), std::get<1>(vel[2]),std::get<1>(pos[3]), std::get<1>(vel[3]));
        auto zs = std::tuple(std::get<2>(pos[0]), std::get<2>(vel[0]),std::get<2>(pos[1]), std::get<2>(vel[1]),std::get<2>(pos[2]), std::get<2>(vel[2]),std::get<2>(pos[3]), std::get<2>(vel[3]));
        
        auto xit = xhist.find(xs); if(xit != xhist.end()) { xcyc = iter - xit->second; std::cout << "cycle in x : " << xit->second << " -> " << iter << " " << (fx=true) << std::endl; }
        auto yit = yhist.find(ys); if(yit != yhist.end()) { ycyc = iter - yit->second; std::cout << "cycle in y : " << yit->second << " -> " << iter << " " << (fy=true) << std::endl; }
        auto zit = zhist.find(zs); if(zit != zhist.end()) { zcyc = iter - zit->second; std::cout << "cycle in z : " << zit->second << " -> " << iter << " " << (fz=true) << std::endl; }

        xhist[xs] = iter;
        yhist[ys] = iter;
        zhist[zs] = iter;



        if(iter == 1000) {
          for(int a = 0; a < pos.size(); a++) {
            auto [x,y,z] = pos[a];
            auto [dx,dy,dz] = vel[a];
            ans1 += (std::abs(x)+std::abs(y)+std::abs(z))*(std::abs(dx)+std::abs(dy)+std::abs(dz));
          }
        }

        if(fx&&fy&&fz) break;

    }
    ans2 = std::lcm(std::lcm(xcyc, ycyc), zcyc);
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p12(std::cin);
}
