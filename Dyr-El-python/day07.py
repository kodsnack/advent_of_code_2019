## Start of header boilerplate #################################################

from aocbase import readInput
import re
import itertools

def lineParse(s, f, fp):
    m = fp.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp, f=lineParse, ff=lambda x:x, fp=re.compile(r"^(.*)$")):
    return tuple(map(lambda x:f(x, ff, fp), inp.splitlines()))

## End of header boilerplate ###################################################

class Comp:
    def readVal(self, argNo):
        if self.mem[self.pc]//(10*10**argNo)%10==1:
            return self.mem[self.pc+argNo]
        else:
            return self.mem[self.mem[self.pc+argNo]]

    def addI(self):
        target = self.mem[self.pc+3]
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        self.mem[target] = arg1 + arg2
        return self.pc+4

    def mulI(self):
        target = self.mem[self.pc+3]
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        self.mem[target] = arg1 * arg2
        return self.pc+4

    def inpI(self):
        if len(self.input) == 0:
            self.state = "waiting"
            return self.pc
        target = self.mem[self.pc+1]
        val = self.input[0]
        del self.input[0]
        self.mem[target] = val
        return self.pc+2

    def outI(self):
        arg1 = self.readVal(1)
        self.output.append(arg1)
        return self.pc + 2

    def jitI(self):
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        if arg1 != 0:
            return arg2
        return self.pc+3

    def jifI(self):
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        if arg1 == 0:
            return arg2
        return self.pc+3

    def ltI(self):
        target = self.mem[self.pc+3]
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        if arg1 < arg2:
            self.mem[target] = 1
        else:
            self.mem[target] = 0
        return self.pc+4

    def eqI(self):
        target = self.mem[self.pc+3]
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        if arg1 == arg2:
            self.mem[target] = 1
        else:
            self.mem[target] = 0
        return self.pc+4

    def halt(self):
        self.state = "halted"

    def getInstr(self):
        return self.mem[self.pc] % 100

    ilist = {1:addI, 2:mulI, 3:inpI, 4:outI, 5:jitI, 6:jifI, 7:ltI, 8:eqI, 99:halt}

    def __init__(self, pinp):
        self.mem = {address: value for address, value in enumerate(pinp[0][0])}
        self.pc = 0
        self.state = "ready"
        self.input = []
        self.output = []
        self.error = []
    def ready(self):
        return self.state == "ready"
    def waiting(self):
        return self.state == "waiting"
    def halted(self):
        return self.state == "halted"
    def inp(self, i):
        self.input.append(i)
        if self.state == "waiting":
            self.state = "ready"
    def out(self):
        ret = self.output[0]
        del self.output[0]
        return ret
    def outEmpty(self):
        return len(self.output)==0
    def step(self):
        if self.halted():
            return
        instr = self.getInstr()
        if instr in self.ilist:
            self.pc = self.ilist[instr](self)
        else:
            self.error.append((self.pc, "Invalid instruction []".format(self.mem[self.pc])))
            self.state = "halted"

def run5thrusters(pinp, inp):
    thrusters = []
    for i in inp:
        thruster = Comp(pinp)
        thruster.inp(i)
        thrusters.append(thruster)
    thrusters[0].inp(0)
    while True:
        noHalted = 0
        for i in range(5):
            prevThruster = thrusters[(i+4)%5]
            thisThruster = thrusters[i]
            nextThruster = thrusters[(i+1)%5]
            if thisThruster.halted():
                noHalted += 1
                continue
            if not prevThruster.outEmpty():
                thisThruster.inp(prevThruster.out())
            thisThruster.step()
            if thisThruster.halted():
                noHalted += 1
        if noHalted == 5:
            break
    return thrusters[4].output[0]

def part1(pinp):
    return max(map(lambda x:run5thrusters(pinp, x),
                   itertools.permutations(range(0, 5))))

def part2(pinp):
    return max(map(lambda x:run5thrusters(pinp, x),
                   itertools.permutations(range(5, 10))))

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    # inp = """3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, ff=lambda x:tuple(map(int, x.split(','))))

    print(part1(parseInp))
    print(part2(parseInp))

## End of footer boilerplate ###################################################
