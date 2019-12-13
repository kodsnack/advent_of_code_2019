import sets
import strutils

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
        echo("Asteroid ", baseAsteroid, " seen: ", seen)
        result = max(result, seen)