#include <iostream>
#include <vector>
#include <tuple>
#include <queue>
#include <algorithm>

void p05(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    const std::vector<int> input = [](auto & is){
        bool done = false;
        int num = 0;
        bool have_num = false;
        bool neg = false;
        std::vector<int> nums;
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
            } else if(c == '-' && !have_num) {
                neg = true;
            } else {
                if(have_num) {
                    nums.push_back(neg ? -num : num);
                }
                neg = false;
                have_num = false;
                num = 0;
            }

        }
        return nums;
    }(is);

    auto progdata = [](int prob) { return prob == 1 ? 1 : 5; };

    auto ans = [&](int prob) -> auto & { return prob == 1 ? ans1 : ans2 ;};
    std::vector<int> phase = {5,6,7,8,9};
    int maxt = 0;
    do {
        int output = 0;
        bool terminated = false;
        std::vector<std::remove_const_t<decltype(input)>> state;
        std::vector<std::remove_const_t<decltype(input)>::size_type> poss;
        std::vector<std::queue<int>> qq;
        for(int i=0; i<5; i++) {
            state.push_back(input);
            poss.push_back(0);
            qq.emplace_back();
            qq.back().push(phase[i]);
        }
        qq[0].push(0);
        while(!terminated) {
            for (int prog = 0; prog < 5; prog++) {
                auto & data = state[prog];
                auto & pos = poss[prog];
                auto & q = qq[prog];
                bool done = false;
                while (!done) {
                    auto op = data[pos] % 100;
                    auto imm1 = (data[pos] / 100) % 10;
                    auto imm2 = (data[pos] / 1000) % 10;
                    auto imm3 = (data[pos] / 10000) % 10;

                    auto a1 = [&]() -> auto & { return data[imm1 ? pos + 1 : data[pos + 1]]; };
                    auto a2 = [&]() -> auto & { return data[imm2 ? pos + 2 : data[pos + 2]]; };
                    auto a3 = [&]() -> auto & { return data[imm3 ? pos + 3 : data[pos + 3]]; };

                    switch (op) {
                        case 1:
                            a3() = a1() + a2();
                            pos += 4;
                            break;
                        case 2:
                            a3() = a1() * a2();
                            pos += 4;
                            break;
                        case 3:
                            if(q.empty()) {
                                std::cout << "nothing in q" << std::endl;
                                exit(0);
                            }
                            a1() = q.front();
                            q.pop();
                            pos += 2;
                            break;
                        case 4:
                            output = a1();
                            pos += 2;
                            qq[(prog+1)%5].push(output);
                            done = true;
                            break;
                        case 5:
                            if (a1()) {
                                pos = a2();
                            } else {
                                pos += 3;
                            }
                            break;
                        case 6:
                            if (!a1()) {
                                pos = a2();
                            } else {
                                pos += 3;
                            }
                            break;
                        case 7:
                            if (a1() < a2()) {
                                a3() = 1;
                            } else {
                                a3() = 0;
                            }
                            pos += 4;
                            break;
                        case 8:
                            if (a1() == a2()) {
                                a3() = 1;
                            } else {
                                a3() = 0;
                            }
                            pos += 4;
                            break;
                        case 99:
                            done = true;
                            if(prog == 4) terminated = true;
                            break;

                        default:
                            std::cout << "unknown op " << op << std::endl;
                            exit(-1);
                    }
                }
            }
        }
        if (output > maxt) {
            std::cout << output << " ";
            maxt = output;
            for (auto c : phase) {
                std::cout << c;
            }
            std::cout << std::endl;
        }
    } while(std::next_permutation(phase.begin(), phase.end()));

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
// 02143 fel
int main() {
    p05(std::cin);
}
