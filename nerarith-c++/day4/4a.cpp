#include <bits/stdc++.h>
using namespace std;

int main() {
    int lower_bound, upper_bound;
    char delimeter;
    int number_valid = 0;
    cin >> lower_bound >> delimeter >> upper_bound;
    for (int number=lower_bound; number <= upper_bound; number++) {
       string password = to_string(number); 
       if (!is_sorted(password.begin(), password.end())) continue;
       string reduced_copy = password;
       if (unique(reduced_copy.begin(), reduced_copy.end()) == reduced_copy.end()) continue;
       number_valid++;
    }
    cout << number_valid << endl;
}
