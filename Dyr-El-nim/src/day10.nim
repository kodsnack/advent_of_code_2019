import sets
import strutils

type
    Point = tuple[x:int, y: int]
    Points = seq[Point]
    PointSet = HashSet[Point] 

proc parseAsteroidMap(theMap: string): Points =
    result = newSeqOfCap[Point](theMap.count('#'))
    for y, line in theMap.splitLines().pairs():
        for x, c in line.pairs():
            if c == '#':
                result.add((x:x, y:y))

proc `-`(lhs, rhs: Point): Point =
    result.x = lhs.x - rhs.x
    result.y = lhs.y - rhs.y

proc norm(p: Point): Point =
    while result.y != 0:
        result = (x: result.y, y: result.x mod result.y)
    result = (x: p.x div result.x, y: p.y div result.x)

proc findBestAsteroid*(theMap: string): int =
    let
        asteroids = parseAsteroidMap(theMap)
    for baseAsteroid in asteroids:
        for otherAsteroid in asteroids:
            let
                dist = otherAsteroid - baseAsteroid
                vector = dist.norm
    result = asteroids.len