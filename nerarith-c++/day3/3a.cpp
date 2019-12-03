#include <bits/stdc++.h>
using namespace std;
map<char, int> dx = {{'U', 1}, {'D', -1}, {'R', 0}, {'L', 0}};
map<char, int> dy = {{'U', 0}, {'D', 0}, {'R', 1}, {'L', -1}};

set<pair<int, int>> read_follow_path () {
    set<pair<int, int>> points;
    char direction, comma;
    int distance;
    int x = 0, y = 0;
    while (true) {
        cin >> direction >> distance;
        for (int i=1; i <= distance; i++) {
            x += dx[direction];
            y += dy[direction];
            points.insert({x,y});
        }
        if (cin.peek() == ',')
            cin >> comma;
        else {
            return points;
        }
    }
}

int main() {
    set<pair<int, int>> pointsA = read_follow_path();
    set<pair<int, int>> pointsB = read_follow_path();
    vector<pair<int, int>> common_points (max(pointsA.size(), pointsB.size()));
    vector<pair<int, int>>::iterator new_end = set_intersection(pointsA.begin(), pointsA.end(), pointsB.begin(), pointsB.end(), common_points.begin());
    common_points.resize(new_end - common_points.begin());
    
    int shortest_distance = INT_MAX;
    for (pair<int, int> point : common_points) {
        shortest_distance = min(shortest_distance, abs(point.first) + abs(point.second));
    }
    cout << shortest_distance << endl;
}
