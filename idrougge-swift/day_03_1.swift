import Foundation

enum Direction {
    case up(Int)
    case down(Int)
    case left(Int)
    case right(Int)
}

extension Direction: LosslessStringConvertible {
    
    var description: String {
        switch self {
        case .up(let steps): return "U\(steps)"
        case .down(let steps): return "D\(steps)"
        case .left(let steps): return "L\(steps)"
        case .right(let steps): return "R\(steps)"
        }
    }
    
    init?(_ description: String) {
        guard let _ = Int(description.dropFirst()) else { return nil }
        self.init(stringLiteral: description)
    }
}

extension Direction: ExpressibleByStringLiteral {
    typealias StringLiteralType = String
    
    init(stringLiteral string: String) {
        guard let steps = Int(string.dropFirst()) else { fatalError("Invalid input: \(string)") }
        switch string.first {
        case "U": self = .up(steps)
        case "D": self = .down(steps)
        case "L": self = .left(steps)
        case "R": self = .right(steps)
        default: fatalError("Invalid input: \(string)")
        }
    }
}

enum Line {
    case horizontal(x: ClosedRange<Int>, y: Int)
    case vertical(x: Int, y: ClosedRange<Int>)
}

extension Line {
    func isOpposingAxis(to other: Line) -> Bool {
        switch (self, other) {
        case (.horizontal, .horizontal): return false
        case (.vertical, .vertical): return false
        default: return true
        }
    }
    
    func overlaps(_ other: Line) -> Bool {
        return crosspoint(with: other) != nil
        switch (self, other) {
        case let (.horizontal(x: hRange, y: y), .vertical(x: x, y: vRange)) where hRange ~= x && vRange ~= y:
            return true
        case let (.vertical(x: x, y: vRange), .horizontal(x: hRange, y: y)) where hRange ~= x && vRange ~= y:
            return true
        default: return false
        }
    }
    
    func crosspoint(with other: Line) -> Point? {
        switch (self, other) {
        case (.horizontal(x: _, y: 0), .vertical(x: 0, y: _)) :
            return .none
        case (.vertical(x: 0, y: _), .horizontal(x: _, y: 0)):
            return .none
        case let (.horizontal(x: hRange, y: y), .vertical(x: x, y: vRange)) where hRange ~= x && vRange ~= y:
            return (x, y)
        case let (.vertical(x: x, y: vRange), .horizontal(x: hRange, y: y)) where hRange ~= x && vRange ~= y:
            return (x, y)
        default:
            return .none
        }
    }
}

typealias Point = (x: Int, y: Int)

let directions = try String(contentsOfFile: NSHomeDirectory()+"/Documents/day3.txt")
//    let directions = """
//                     R8,U5,L5,D3
//                     U7,R6,D4,L4
//                     """
//let directions = """
//R75,D30,R83,U83,L12,D49,R71,U7,L72
//U62,R66,U55,R34,D71,R55,D58,R83
//"""
//let directions = """
//R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
//U98,R91,D20,R16,D67,R40,U7,R15,U6,R7
//"""
    .components(separatedBy: .newlines)
    .map { $0.components(separatedBy: ",").compactMap(Direction.init) }


typealias State = (point: Point, directions: IndexingIterator<[Direction]>)

func lineGenerator(directions: [Direction]) -> UnfoldSequence<Line, State> {
    var iterator = directions.makeIterator()
    return sequence(state: ((0,0), iterator)) { (state: inout State) -> Line? in
        guard let direction = iterator.next() else { return nil }
        print(direction)
        defer {
            print(state.point)
        }
        let from = state.point
        switch direction {
        case let .up(steps):
            state.point.y -= steps
            return .vertical(x: state.point.x, y: state.point.y...from.y)
        case let .down(steps):
            state.point.y += steps
            return .vertical(x: state.point.x, y: from.y...state.point.y)
        case let .left(steps):
            state.point.x -= steps
            return .horizontal(x: state.point.x...from.x, y: state.point.y)
        case let .right(steps):
            state.point.x += steps
            return .horizontal(x: from.x...state.point.x, y: state.point.y)
        }
    }
}
let first = lineGenerator(directions: directions[0]).map{$0}
let second = lineGenerator(directions: directions[1]).map{$0}

second.reduce(into: Int.max) { (nearest, line) in
    let match = first.filter({ it -> Bool in
        it.overlaps(line)
    }).sorted(by: { (this, that) -> Bool in
        guard
            let a = this.crosspoint(with: line),
            let b = that.crosspoint(with: line)
            else { return false }
        return abs(a.x) + abs(a.y) < abs(b.x) + abs(b.y)
        return a < b
        switch (this, that) {
        case let (.horizontal(x: _, y: y1), .horizontal(x: _, y: y2)):
            return abs(y1) < abs(y2)
        case let (.vertical(x: x1, y: _), .vertical(x: x2, y: _)):
            return abs(x1) < abs(x2)
        default: return false
        }
    }).first
    guard let match_ = match else { return }
    print(line, match_, match_.crosspoint(with: line))
    guard let pt = match_.crosspoint(with: line) else { return }
    nearest = min(nearest, abs(pt.x) + abs(pt.y))
}
