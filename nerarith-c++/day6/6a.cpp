#include <bits/stdc++.h>
using namespace std;

int main() {
    map<string, vector<string>> orbited_by;
    string input_line;
    while (true) {
        string body1, body2;
        getline(cin, body1, ')');
        if (body1 == "") break;
        getline(cin, body2);
        orbited_by[body1].push_back(body2);
    }
    queue<string> bodies_to_count;
    bodies_to_count.push("COM");
    map<string, int> number_indirect_orbits;
    number_indirect_orbits["COM"] = -1;
    int total_orbits = 0;
    while (!bodies_to_count.empty()) {
        string current_body = bodies_to_count.front(); bodies_to_count.pop();
        total_orbits += 1 + number_indirect_orbits[current_body];
        for (string orbiting_body : orbited_by[current_body]) {
            number_indirect_orbits[orbiting_body] = 1 + number_indirect_orbits[current_body];
            bodies_to_count.push(orbiting_body);
        }
    }
    cout << total_orbits << endl;
}
