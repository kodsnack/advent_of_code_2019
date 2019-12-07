
def GetTotalFromChildren(current, map_data, level):
    if current not in map_data:
        return level

    children = map_data[current]
    s = level
    for child in children:
        s += GetTotalFromChildren(child, map_data, level + 1)
    return s

map_data = dict()

f = open("6dec-input", 'r')
for line in f:
    [a, b] = line.split("\n")[0].split(")")

    if a not in map_data:
        map_data[a] = []

    map_data[a].append(b)

f.close()

total = GetTotalFromChildren("COM", map_data, 0)

print("Part 1: %d" % total)

santa = None
you = None

edges = dict()

# we can jump in any direction so expand list of edges
for key in map_data:
    if key not in edges:
        edges[key] = []
    children = map_data[key]
    for child in children:
        if child == "SAN":
            santa = key
        elif child == "YOU":
            you = key
        else:
            edges[key].append(child)
            if child not in edges:
                edges[child] = []
            if key not in edges[child]:
                edges[child].append(key)

def getCost(path):
    (cost, current) = path
    return cost

def getNode(path):
    (cost, node) = path
    return node

targets = []
for child in edges[you]:
    targets.append((1, child))

visited = dict()
# Lets do dijkstras
while True:
    targets.sort(key=getCost)
    current = targets.pop(0)
    while getNode(current) in visited:
        current = targets.pop(0)

    cost = getCost(current)
    visited[getNode(current)] = cost

    if getNode(current) == santa:
        break

    for child in edges[getNode(current)]:
        targets.append((cost + 1, child))

print("Part 2: %s for cost %d" % (getNode(current), getCost(current)))
