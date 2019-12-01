#include <bits/stdc++.h>
using namespace std;

int main() {
    int total_fuel_required = 0;
    int mass;
    while (cin >> mass) {
        int new_fuel_required = (mass / 3) - 2;
        do {
            total_fuel_required += new_fuel_required;
            new_fuel_required = (new_fuel_required / 3) - 2;
        } while (new_fuel_required > 0);
    }
    cout << total_fuel_required << endl;
}
