import sets
import strutils
import sequtils
import algorithm

type
    Point = tuple[x:int, y: int]
    Points = seq[Point]
    PointSet = HashSet[Point] 

proc parseAsteroidMap(theMap: string): PointSet =
    init(result) 
    for y, line in theMap.splitLines().pairs():
        for x, c in line.strip().pairs():
            if c == '#':
                result.incl((x:x, y:y))

proc `-`(lhs, rhs: Point): Point =
    result.x = lhs.x - rhs.x
    result.y = lhs.y - rhs.y

proc `+`(lhs, rhs: Point): Point =
    result.x = lhs.x + rhs.x
    result.y = lhs.y + rhs.y

proc sgd(a, b: int): int =
    result = a
    var
        i = b
    while i != 0:
        let temp = result
        result = i
        i = temp mod i

proc norm(p: Point): Point =
    let normDiv = sgd(abs(p.x), abs(p.y))
    result = (x: p.x div normDiv, y: p.y div normDiv)

proc findBestAsteroid*(theMap: string): int =
    let
        asteroids = parseAsteroidMap(theMap)
    for baseAsteroid in asteroids:
        var seen = 0
        for otherAsteroid in asteroids:
            if baseAsteroid == otherAsteroid:
                continue
            let
                dist = otherAsteroid - baseAsteroid
                vector = dist.norm
            var
                hidden = false
                p = baseAsteroid + vector
            while not hidden and p != otherAsteroid:
                hidden = hidden or asteroids.contains(p)
                p = p + vector
            if not hidden:
                seen.inc
        result = max(result, seen)

proc findBestAsteroidCoords(asteroids: PointSet): Point =
    var bestSeen = 0
    for baseAsteroid in asteroids:
        var seen = 0
        for otherAsteroid in asteroids:
            if baseAsteroid == otherAsteroid:
                continue
            let
                dist = otherAsteroid - baseAsteroid
                vector = dist.norm
            var
                hidden = false
                p = baseAsteroid + vector
            while not hidden and p != otherAsteroid:
                hidden = hidden or asteroids.contains(p)
                p = p + vector
            if not hidden:
                seen.inc
        if seen > bestSeen:
            bestSeen = seen
            result = baseAsteroid

proc sweepOrder(v: Point): float64 =
    result = v.y * abs(v.y) / (v.x * v.x + v.y * v.y)
    if v.x < 0:
        result = 2 - result

proc findMin(ps: PointSet): Point =
    for p in ps:
        result.x = min(result.x, p.x)
        result.y = min(result.y, p.y)

proc findMax(ps: PointSet): Point =
    for p in ps:
        result.x = max(result.x, p.x)
        result.y = max(result.y, p.y)
        
proc numberVaporised*(theMap: string, number: int): int =
    var asteroids = parseAsteroidMap(theMap)
    let minPoint = findMin(asteroids)
    let maxPoint = findMax(asteroids)
    let baseAsteroid = findBestAsteroidCoords(asteroids)
    asteroids.excl(baseAsteroid)
    var vectorSet: PointSet
    vectorSet.init()
    for a in asteroids:
        vectorSet.incl((a - baseAsteroid).norm())
    var vectorList: Points = toSeq(vectorSet.items())
    vectorList.sort do (v1, v2: Point) -> int:
        result = cmp(v1.sweepOrder, v2.sweepOrder)
    var killed = 0
    while true:
        for v in vectorList:
            var p = baseAsteroid
            while (p.x >= minPoint.x and p.x <= maxPoint.x and 
                   p.y >= minPoint.y and p.y <= maxPoint.y):
                p = p + v
                if p in asteroids:
                    asteroids.excl(p)
                    killed.inc
                    if killed == number:
                        return p.x*100+p.y
                    break