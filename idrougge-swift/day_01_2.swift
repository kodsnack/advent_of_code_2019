#!/usr/bin/env xcrun swift

import Foundation

guard CommandLine.arguments.count == 2, CommandLine.arguments[1].isEmpty == false
    else { 
        print("Specify file name")
        exit(0)
}

func fuel(for mass: Int) -> Int? {
    let mass = mass / 3 - 2
    guard mass > 0 else { return nil }
    return mass
}

let mass = try! String(contentsOfFile: CommandLine.arguments[1])
    .components(separatedBy: .newlines)
    .compactMap(Int.init)
    .compactMap(fuel)
    .lazy
    .flatMap { sequence(first: $0, next: fuel) }
    .reduce(0, +)

print(mass)
