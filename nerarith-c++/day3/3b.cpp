#include <bits/stdc++.h>
using namespace std;
map<char, int> dx = {{'U', 1}, {'D', -1}, {'R', 0}, {'L', 0}};
map<char, int> dy = {{'U', 0}, {'D', 0}, {'R', 1}, {'L', -1}};

const int num_paths = 2;

struct Point {
    vector<int> distances = vector(num_paths, INT_MAX);
    void set_distance (int path_index, int distance) {
        distances[path_index] = min(distances[path_index], distance);
    }
    bool is_intersection () {
        for (int distance : distances)
            if (distance == INT_MAX)
                return false;
        return true;
    }
    int total_distance () {
        return accumulate(distances.begin(), distances.end(), 0);
    }
};

map<pair<int, int>, Point> points;

void read_follow_path (int path_index) {
    char direction, comma;
    int distance, total_distance = 0;
    int x = 0, y = 0;
    while (true) {
        cin >> direction >> distance;
        for (int i=1; i <= distance; i++) {
            x += dx[direction];
            y += dy[direction];
            total_distance++;
            points[{x,y}].set_distance(path_index, total_distance);
        }
        if (cin.peek() != ',')
            return;
        cin >> comma;
    }
}

int main() {
    for (int path_index=0; path_index < num_paths; path_index++)
        read_follow_path(path_index);
    int shortest_distance = INT_MAX;
    for (auto [coordinates, point] : points) {
        if (point.is_intersection())
            shortest_distance = min(shortest_distance, point.total_distance());
    }
    cout << shortest_distance << endl;
}
