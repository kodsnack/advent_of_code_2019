
def parse(step):
    direction = step[0]
    distance = int(step[1:])
    delta = (0,0)

    if direction == "D":
        delta = (0,-distance)
    elif direction == "U":
        delta = (0, distance)
    elif direction == "L":
        delta = (-distance, 0)
    elif direction == "R":
        delta = (distance, 0)

    return delta

def add(a, b):
    (ax, ay) = a
    (bx, by) = b
    c = (ax + bx, ay + by)
    return c

def move_positive(start, end):
    (s_x, s_y) = start
    (e_x, e_y) = end

    if s_x == e_x and s_y > e_y:
        return (end, start)
    if s_y == e_y and s_x > e_x:
        return (end, start)
    return (start, end)

def intersection(a_start, a_end, b_start, b_end):
    (a_start, a_end) = move_positive(a_start, a_end)
    (b_start, b_end) = move_positive(b_start, b_end)

    (as_x, as_y) = a_start
    (ae_x, ae_y) = a_end
    (bs_x, bs_y) = b_start
    (be_x, be_y) = b_end

    # a moves in X and b moves in Y
    if as_x < bs_x and ae_x > bs_x and bs_y < as_y and be_y > as_y:
        return (bs_x, as_y)

    # a moves in Y and b moves in X
    if bs_x < as_x and be_x > as_x and as_y < bs_y and ae_y > bs_y:
        return (as_x, bs_y)

    return False

def manhattan_to_origin(point):
    (x, y) = point
    if x < 0:
        x = 0 - x
    if y < 0:
        y = 0 - y

    manhattan = x + y
    return manhattan

def trace(path):
    pathway = []
    current_position = (0,0)

    for step in path:
        delta = parse(step)
        current_position = add(current_position, delta)
        pathway.append(current_position)

    return pathway

def crossings(path_a, path_b):
    cross = []

    a = 0
    b = 0
    while(a < len(path_a) - 1):
        while(b < len(path_b) - 1):
            crossing = intersection(path_a[a], path_a[a + 1], path_b[b], path_b[b + 1])
            if crossing:
                delta = manhattan_to_origin(crossing)
                cross.append((delta, crossing))
            b += 1
        a += 1
        b = 0
    return cross

def shortest_crossing(crossings):
    def compare_distance(a):
        (delta_a, point_a) = a
        return delta_a

    crossings.sort(key = compare_distance)
    return crossings[0]


file_content = open("3dec-input", 'r')
#file_content = ["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"]
#file_content = ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
#file_content = ["R8,U5,L5,D3", "U7,R6,D4,L4"]
paths = []

for line in file_content:
    paths.append(trace(line.split(",")))

cross = crossings(paths[0], paths[1])
print(shortest_crossing(cross))

file_content.close()
