#include <iostream>
#include <vector>
#include <tuple>
#include <queue>

void p07(std::istream & is) {
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

    for(const auto problem : {1,2}) {
        auto data = input;

        decltype(data)::size_type pos = 0;
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
                    a1() = progdata(problem);
                    pos += 2;
                    break;
                case 4:
                    ans(problem) = a1();
                    pos += 2;
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
                    break;

                default:
                    std::cout << "unknown op " << op << std::endl;
                    exit(-1);
            }
        }
    }
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p07(std::cin);
}
