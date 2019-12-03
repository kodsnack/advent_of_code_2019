import strutils

type
    Line = tuple[vertical: bool, x: int, y: int, len: int, steps: int]

proc steps(s:string): seq[Line] =
    result = newSeqOfCap[Line](s.len)
    var
        x, y, stepsSoFar: int
    for step in s.split(','):
        let
            directionChar = step[0]
            length = step.substr(1, step.len).parseInt()
            vertical = directionChar == 'U' or directionChar == 'D'
        if directionChar == 'D' or directionChar == 'L':
            result.add((vertical: vertical, x: x, y: y, len: -length, steps: stepsSoFar))
            if vertical:
                y.dec(length)
            else:
                x.dec(length)
        else:
            result.add((vertical: vertical, x: x, y: y, len: length, steps: stepsSoFar))
            if vertical:
                y.inc(length)
            else:
                x.inc(length)
        stepsSoFar.inc(length)

proc minx(segment: Line): int =
    result = segment.x
    if not segment.vertical and segment.len < 0:
        result.dec(segment.len)

proc maxx(segment: Line): int =
    result = segment.x
    if not segment.vertical and segment.len > 0:
        result.inc(segment.len)
        
proc miny(segment: Line): int =
    result = segment.y
    if segment.vertical and segment.len < 0:
        result.dec(segment.len)

proc maxy(segment: Line): int =
    result = segment.y
    if segment.vertical and segment.len > 0:
        result.inc(segment.len)
    
proc distanceToOrigo(segment1, segment2: Line): int =
    let
        x1min = minx(segment1)
        y1min = miny(segment1)
        x1max = maxx(segment1)
        y1max = maxy(segment1)
        x2min = minx(segment2)
        y2min = miny(segment2)
        x2max = maxx(segment2)
        y2max = maxy(segment2)
        x3max = min(x1max, x2max)
        x3min = max(x1min, x2min)
        y3max = min(y1max, y2max)
        y3min = max(y1min, y2min)
    if x3min > x3max or y3min > y3max:
        result = high(int)
    else:
        if x3min > 0:
            result += x3min
        elif x3max < 0:
            result -= x3max
        if y3min > 0:
            result += y3min
        elif y3max 

proc findMinDistance*(s: string): int =
    let
        lines = s.splitLines()
        firstWire = steps(lines[0])
        secondWire = steps(lines[1])
    for segment1 in firstWire:
        for segment2 in secondWire:
            result = min(result, distanceToOrigo(segment1, segmen2))
    result = len(firstWire) + len(secondWire)
    echo(firstWire)
    echo(secondWire)

    