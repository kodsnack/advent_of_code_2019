import Foundation

let targetValue = 19690720

enum Opcode: Int {
    case add = 1, mul = 2, rts = 99
}

let seeds = sequence(first: (0, 0)) { (noun, verb) in
    switch (noun, verb) {
    case (99, 99): return nil
    case (_, 99): return (noun + 1, 0)
    case (_, _): return (noun, verb + 1)
    }
}

func run(_ memory: [Int], noun: Int, verb: Int) -> Int {
    var ram = memory
    ram[1] = noun
    ram[2] = verb
    for pc in stride(from: ram.startIndex, to: ram.endIndex, by: 4) {
        let (src1, src2, dest) = (ram[pc+1], ram[pc+2], ram[pc+3])
        switch Opcode(rawValue: ram[pc]) {
        case .add?: ram[dest] = ram[src1] + ram[src2]
        case .mul?: ram[dest] = ram[src1] * ram[src2]
        case .rts?: return ram[0]
        case .none: fatalError("Illegal opcode @ \(pc): \(ram[pc]) \n\(ram)")
        }
    }
    return ram[0]
}


let memory = try String(contentsOfFile: NSHomeDirectory()+"/Documents/day2.txt")
    .components(separatedBy: ",")
    .compactMap(Int.init)


let seed = seeds.first(where: { (n, v) in run(memory, noun: n, verb: v) == targetValue })
let part2 = seed.map { $0.0 * 100 + $0.1 }
print(part2!)
