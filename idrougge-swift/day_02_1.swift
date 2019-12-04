import Foundation

enum Opcode: Int {
    case add = 1, mul = 2, rts = 99
}

func run(_ memory: [Int]) -> Int {
    var ram = memory
    for pc in stride(from: ram.startIndex, to: ram.endIndex, by: 4) {
        guard
            let op = Opcode(rawValue: ram[pc])
            else { fatalError("Illegal opcode @ \(pc): \(ram[pc]) \n\(ram)") }
        let (src1, src2, dest) = (ram[pc+1], ram[pc+2], ram[pc+3])
        switch op {
        case .add: ram[dest] = ram[src1] + ram[src2]
        case .mul: ram[dest] = ram[src1] * ram[src2]
        case .rts: return ram[0]
        }
    }
    return ram[0]
}

var memory = try String(contentsOfFile: NSHomeDirectory()+"/Documents/day2.txt")
    .components(separatedBy: ",")
    .compactMap(Int.init)

memory[1] = 12
memory[2] = 02

print(run(memory))
