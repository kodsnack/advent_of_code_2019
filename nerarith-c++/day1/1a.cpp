#include <bits/stdc++.h>
using namespace std;

int main() {
    int total_fuel_required = 0;
    int mass;
    while (cin >> mass)
        total_fuel_required += (mass / 3) - 2;
    cout << total_fuel_required << endl;
}
