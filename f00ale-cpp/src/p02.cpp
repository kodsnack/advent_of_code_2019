#include <iostream>
#include <vector>
#include <tuple>

void p02(std::istream & is) {
    int ans1 = 0;
    int ans2 = 0;
    const std::vector<int> input = [](auto & is){
        bool done = false;
        int num = 0;
        bool have_num = false;
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
            } else {
                if(have_num) {
                    nums.push_back(num);
                }
                have_num = false;
                num = 0;
            }

        }
        return nums;
    }(is);

    for(int noun = 0; noun < 100; noun++) {
        for(int verb = 0; verb < 100; verb++) {
            auto data = input;
            data[1] = noun;
            data[2] = verb;
            int pos = 0;
            bool done = false;
            while (!done) {
                switch (data[pos]) {
                    case 1:
                        data[data[pos + 3]] = data[data[pos + 2]] + data[data[pos + 1]];
                        pos += 4;
                        break;
                    case 2:
                        data[data[pos + 3]] = data[data[pos + 2]] * data[data[pos + 1]];
                        pos += 4;
                        break;
                    case 99:
                        done = true;
                        break;

                    default:
                        exit(-1);
                }
            }
            if(noun == 12 && verb == 2) {
                ans1 = data[0];
            }
            if(data[0] == 19690720) {
                ans2 = 100*noun + verb;
            }
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}

int main() {
    p02(std::cin);
}
