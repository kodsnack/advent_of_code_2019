#include <bits/stdc++.h>
using namespace std;

string FROM = "YOU";
string TO = "SAN";

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
    map<string, vector<string>> orbiting;   // both directly and indirectly
    
    orbiting["COM"] = {};
    while (orbiting[FROM].empty() || orbiting[TO].empty()) {
        string current_body = bodies_to_count.front(); bodies_to_count.pop();
        vector<string> indirectly_orbiting = orbiting[current_body];
        
        for (string orbiting_body : orbited_by[current_body]) {
            indirectly_orbiting.push_back(orbiting_body);
            orbiting[orbiting_body] = indirectly_orbiting;
            indirectly_orbiting.pop_back();
            
            bodies_to_count.push(orbiting_body);
        }
    }

    vector<string> path_from = orbiting[FROM];
    vector<string> path_to = orbiting[TO];
    int distance = (path_to.size() - 1) + (path_from.size() - 1);
    for (int i=0; i < path_from.size() && i < path_to.size() && path_from[i] == path_to[i]; i++)
        distance -= 2;
    cout << distance << endl;
}
