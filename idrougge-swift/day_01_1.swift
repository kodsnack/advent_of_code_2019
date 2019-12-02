#!/usr/bin/env xcrun swift

import Foundation

guard CommandLine.arguments.count == 2, CommandLine.arguments[1].isEmpty == false
    else { 
        print("Specify file name")
        exit(0)
}

let mass = try! String(contentsOfFile: CommandLine.arguments[1])
    .components(separatedBy: .newlines)
    .compactMap(Int.init)
    .map { mass in mass / 3 - 2 }
    .reduce(0, +)

print(mass)
