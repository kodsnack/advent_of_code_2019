from collections import defaultdict


def _min_dist(q, dist):
    i = float('inf')
    u = None
    for v in q:
        if dist[v] < i:
            i = dist[v]
            u = v
    assert u
    return u


def djikstra(graph, target, source):
    q = set(graph.keys())
    dist = defaultdict(lambda: float('inf'))
    prev = defaultdict(lambda: None)

    dist[source] = 0
    while q:
        u = _min_dist(q, dist)

        q.remove(u)
        if u == target:
            break

        for v in graph[u]:
            alt = dist[u] + 1
            if alt < dist[v]:
                dist[v] = alt
                prev[v] = u

    return dist, prev
