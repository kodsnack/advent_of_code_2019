#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <set>
#include <map>

void p22(std::istream & is) {
    int64_t ans1 = 0;
    int64_t ans2 = 0;

    std::vector<std::tuple<std::string,int>> cmds;

    {
        bool done = false;
        int num = 0;
        bool neg = false;
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
            } else if((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
                str.push_back(c);
            } else if(c=='-') {
                neg = true;
            } else {
                if(c == '\n') {
                    if(neg) num = -num;
                    if(!str.empty()) {
                        cmds.push_back(std::tuple(str,num));
                    }
                    num = 0;
                    neg = false;
                    str.clear();

                }
            }

        }
    }

/*
    constexpr int Nt = 10;
    std::vector<int> cards(Nt);
    for(size_t i = 0; i < cards.size(); i++) cards[i] = i;

    cmds.clear();
    //cmds.emplace_back("dealwithincrement", 3);
    //cmds.emplace_back("dealwithincrement", 3);
    //cmds.emplace_back("cut", 5);
    cmds.emplace_back("dealwithincrement", 9);

    for (auto[str, num] : cmds) {
        if (str == "dealintonewstack") {
            std::vector<int> ns(cards.rbegin(), cards.rend());
            cards.swap(ns);
        } else if (str == "cut") {
            if (num > 0) {
                std::vector<int> cpy(cards.begin(), cards.begin() + num);
                cards.erase(cards.begin(), cards.begin() + num);
                cards.insert(cards.end(), cpy.begin(), cpy.end());
            } else {
                std::vector<int> cpy(cards.end() + num, cards.end());
                cards.erase(cards.end() + num, cards.end());
                cards.insert(cards.begin(), cpy.begin(), cpy.end());
            }
        } else if (str == "dealwithincrement") {
            std::vector<int> cpy(Nt);
            for (int i = 0; i < Nt; i++) {
                auto toidx = i * num % cpy.size();
                cpy[toidx] = cards[i];
            }
            cards.swap(cpy);
        }
    }
    for(auto n : cards) std::cout << n << " ";
    std::cout << std::endl;
    exit(0);
*/
    auto powmod = [](auto a, auto e, auto m) {
        __int128 ret = 1;
        __int128 b = a;
        b %= m;
        while(b < 0) b+=m;
        while(e) {
            if(e&1) ret = (ret * b) % m;
            e /= 2;
            b = (b*b) % m;
        }
        return ret;
    };


    auto getparam = [powmod, &cmds](int64_t N, int64_t It) {
        int64_t stride = 1, offset = 0;
        for (auto &[cmd, num] : cmds) {
            if (cmd == "dealintonewstack") {
                stride = -stride;
                offset = (offset + stride) % N;
            } else if (cmd == "cut") {
                offset = (offset + stride * num) % N;
            } else if (cmd == "dealwithincrement") {
                // powmod(x, N2-2, N2) becomes inverse modulus
                stride = (stride * powmod(num, N - 2, N)) % N;
            }
        }

        auto total_stride = powmod(stride, It, N);
        auto total_offset = (offset * (1 - total_stride)) % N;
        total_offset *= powmod((1 - stride) % N, N - 2, N);
        total_offset %= N;
        return std::tuple(total_stride, total_offset);
    };

    constexpr int N1 = 10007;
    auto [i1,o1] = getparam(N1, 1);
    for(int c = 0; c < N1; c++) {
        if((o1 + c*i1) % N1 == 2019) {
            ans1 = c;
            break;
        }
    }

    constexpr int64_t N2 = 119315717514047;
    constexpr int64_t ITERS = 101741582076661;

    auto [i2,o2] = getparam(N2, ITERS);

    ans2 = (o2 + 2020 * i2) % N2;

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p22(std::cin);
}
