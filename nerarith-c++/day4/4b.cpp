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
       char last_digit = 0;
       int num_same = 0;
       bool valid = false;
       for (char current_digit : password) {
           if (current_digit == last_digit)
               num_same++;
           else {
               if (num_same == 2)
                   valid = true;
               num_same = 1;
               last_digit = current_digit;
           }
       }
       if (num_same == 2)
           valid = true;
       if (valid)
           number_valid++;
    }
    cout << number_valid << endl;
}
