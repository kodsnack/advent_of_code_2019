#include <iostream>
#include <vector>
#include <tuple>
#include <queue>
#include <algorithm>
#include <numeric>
#include <sstream>

void p09(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    const std::vector<int64_t> input = [](auto &is) {
        bool done = false;
        int64_t num = 0;
        bool have_num = false;
        bool neg = false;
        std::vector<int64_t> nums;
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
                if (have_num) {
                    nums.push_back(neg ? -num : num);
                }
                neg = false;
                have_num = false;
                num = 0;
            }

        }
        return nums;
    }(is);

    auto ans = [&](int prob) -> auto & { return prob == 0 ? ans1 : ans2 ;};
    for(int i = 0; i < 2; i++) {
            int output = 0;
            std::vector<std::remove_const_t<decltype(input)>> states;
            std::vector<std::remove_const_t<decltype(input)>::size_type> positions;
            std::vector<std::remove_const_t<decltype(input)>::size_type> rels;
            std::vector<std::queue<int>> inputs;
            for (int i = 0; i < 5; i++) {
                states.push_back(input);
                positions.push_back(0);
                inputs.emplace_back();
                rels.emplace_back();
                //inputs.back().push(phase[i]);
            }
            //inputs[0].push(0);
                for (int prog = 0; prog < 1; prog++) {
                    auto &data = states[prog];
                    auto &pos = positions[prog];
                    auto &rel = rels[prog];
                    auto &q = inputs[prog];
                    q.emplace(2);
                    bool done = false;
                    int64_t output = 0;
                    while (!done) {
                        auto op = data[pos] % 100;
                        auto imm1 = ((data[pos] / 100) % 10) == 1;
                        auto imm2 = ((data[pos] / 1000)) % 10 == 1;
                        auto imm3 = ((data[pos] / 10000)) % 10 == 1;
                        auto rel1 = ((data[pos] / 100)) % 10 == 2;
                        auto rel2 = ((data[pos] / 1000)) % 10 == 2;
                        auto rel3 = ((data[pos] / 10000)) % 10 == 2;

                        auto a1 = [&]() -> auto & { return data[imm1 ? pos + 1 : (rel1?rel:0)+data[(rel1 ? 0 : 0) + pos + 1]]; };
                        auto a2 = [&]() -> auto & { return data[imm2 ? pos + 2 : (rel2?rel:0)+data[(rel2 ? 0 : 0) + pos + 2]]; };
                        auto a3 = [&]() -> auto & { return data[imm3 ? pos + 3 : (rel3?rel:0)+data[(rel3 ? 0 : 0) + pos + 3]]; };

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
                                if (q.empty()) {
                                    std::cout << "nothing in q" << std::endl;
                                    exit(0);
                                }
                                a1() = q.front();
                                q.pop();
                                pos += 2;
                                break;
                            case 4:
                                output = a1();
                                std::cout << output << std::endl;
                                pos += 2;
                                //inputs[(prog + 1) % 5].push(output);
                                //done = true;
                                //terminated = true;
                                exit(0);
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
                            case 9:
                                rel += a1();
                                pos += 2;
                                break;
                            case 99:
                                done = true;
                                //if (prog == 4) terminated = true;
                                exit(0);
                                break;

                            default:
                                std::cout << "unknown op " << op << std::endl;
                                exit(-1);
                        }
                    }
                }
            }
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p09(std::cin);
    //auto ss = std::stringstream("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99");p07(ss);

}
