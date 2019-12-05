#include <iostream>
#include <vector>
#include <tuple>
#include <queue>

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

    auto data = input;

    std::queue<int> q;
    q.push(5);

    int pos = 0;
    bool done = false;
    while (!done) {
        auto op = data[pos] % 100;
        auto imm1 = (data[pos] / 100) % 10;
        auto imm2 = (data[pos] / 1000) % 10;
        //auto imm3 = (data[pos] / 10000) % 10;
        switch (op) {
            case 1:
                data[data[pos + 3]] = data[imm2 ? pos + 2 : data[pos + 2]] + data[imm1 ? pos + 1 : data[pos + 1]];
                pos += 4;
                break;
            case 2:
                data[data[pos + 3]] = data[imm2 ? pos+2 : data[pos + 2]] * data[imm1 ? pos+1: data[pos + 1]];
                pos += 4;
                break;
            case 3:
                if(q.empty()) {
                    std::cout << "empty queue" << std::endl;
                    exit(0);
                }
                data[data[pos+1]] = q.front();
                q.pop();
                pos += 2;
                break;
            case 4:
                std::cout << data[imm1 ? pos+1 : data[pos+1]] << std::endl;
                pos += 2;
                break;
            case 5:
                if(data[imm1 ?  pos+1 : data[pos+1]]) {
                    pos = data[imm2 ? pos+2 : data[pos+2]];
                } else {
                    pos+= 3;
                }
                break;
            case 6:
                if(!data[imm1 ?  pos+1 : data[pos+1]]) {
                    pos = data[imm2 ? pos+2 : data[pos+2]];
                } else {
                    pos+= 3;
                }
                break;
            case 7:
                if(data[imm1 ? pos+1 : data[pos+1]] < data[imm2 ? pos+2 : data[pos+2]]) {
                    data[data[pos+3]] = 1;
                } else {
                    data[data[pos+3]] = 0;
                }
                pos += 4;
                break;
            case 8:
                if(data[imm1 ? pos+1 : data[pos+1]] == data[imm2 ? pos+2 : data[pos+2]]) {
                    data[data[pos+3]] = 1;
                } else {
                    data[data[pos+3]] = 0;
                }
                pos += 4;
                break;
            case 99:
                done = true;
                break;

            default:
                exit(-1);
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p05(std::cin);
}
