#pragma once

#include <vector>
#include <stdint.h>
#include <iostream>
#include <queue>

std::vector<int64_t> readIntcode(std::istream & is) {
    std::vector<int64_t> ret;
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
}

struct intcodemachine {
    std::vector<int64_t> data;
    std::vector<int64_t>::size_type pos;
    std::vector<int64_t>::size_type rel;
    std::queue<int64_t> inputqueue;
    enum STATE { RUNNING, WAITING, OUTPUT, TERMINATED } state;
    int64_t output;

    intcodemachine(std::vector<int64_t> _data) : data(std::move(_data)), pos{0}, rel{0}, inputqueue{}, state{RUNNING}, output{0} {}

    auto & getdata(size_t addr) {
        if(addr >= data.size()) data.resize(addr+1, 0);
        return data[addr];
    }

    bool step() {
        state = RUNNING;
        const auto op = data[pos] % 100;
        const auto imm1 = ((data[pos] / 100) % 10) == 1;
        const auto imm2 = ((data[pos] / 1000)) % 10 == 1;
        const auto imm3 = ((data[pos] / 10000)) % 10 == 1;
        const auto rel1 = ((data[pos] / 100)) % 10 == 2;
        const auto rel2 = ((data[pos] / 1000)) % 10 == 2;
        const auto rel3 = ((data[pos] / 10000)) % 10 == 2;

        auto a1 = [&]() -> auto & { return getdata(imm1 ? pos + 1 : (rel1?rel:0)+getdata(pos+1)); };
        auto a2 = [&]() -> auto & { return getdata(imm2 ? pos + 2 : (rel2?rel:0)+getdata(pos+2)); };
        auto a3 = [&]() -> auto & { return getdata(imm3 ? pos + 3 : (rel3?rel:0)+getdata(pos+3)); };

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
                if (inputqueue.empty()) {
                    state = WAITING;
                } else {
                    a1() = inputqueue.front();
                    inputqueue.pop();
                    pos += 2;
                }
                break;
            case 4:
                output = a1();
                state = OUTPUT;
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
            case 9:
                rel += a1();
                pos += 2;
                break;
            case 99:
                state = TERMINATED;
                break;

            default:
                std::cout << "unknown op " << op << std::endl;
                exit(-1);
        }
        return state == RUNNING;
    }

    void addInput(int64_t i) { inputqueue.push(i); }
};
