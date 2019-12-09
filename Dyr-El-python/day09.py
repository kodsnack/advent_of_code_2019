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
    def writeTarget(self, argNo):
        mode = self.mem[self.pc]//(10*10**argNo)%10
        if mode == 2:
            return self.mem[self.pc+argNo] + self.relBase
        else:
            return self.mem[self.pc+argNo]
    def readVal(self, argNo):
        mode = self.mem[self.pc]//(10*10**argNo)%10
        if mode == 1:
            return self.mem[self.pc+argNo]
        elif mode == 2:
            return self.mem[self.mem[self.pc+argNo] + self.relBase]
        else:
            return self.mem[self.mem[self.pc+argNo]]

    def addI(self):
        target = self.writeTarget(3)
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        self.mem[target] = arg1 + arg2
        return self.pc+4

    def mulI(self):
        target = self.writeTarget(3)
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        self.mem[target] = arg1 * arg2
        return self.pc+4

    def inpI(self):
        if len(self.input) == 0:
            self.state = "waiting"
            return self.pc
        target = self.writeTarget(1)
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
        target = self.writeTarget(3)
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        if arg1 < arg2:
            self.mem[target] = 1
        else:
            self.mem[target] = 0
        return self.pc+4

    def eqI(self):
        target = self.writeTarget(3)
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        if arg1 == arg2:
            self.mem[target] = 1
        else:
            self.mem[target] = 0
        return self.pc+4

    def aBase(self):
        arg1 = self.readVal(1)
        self.relBase += arg1
        return self.pc+2

    def halt(self):
        self.state = "halted"

    def getInstr(self):
        return self.mem[self.pc] % 100

    ilist = {1:addI, 2:mulI, 3:inpI, 4:outI, 5:jitI, 6:jifI, 7:ltI, 8:eqI,
        9: aBase, 99:halt}

    def __init__(self, pinp):
        self.mem = {address: value for address, value in enumerate(pinp)}
        self.pc = 0
        self.relBase = 0
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
    def run(self):
        while(not self.halted()):
            self.step()

def oneInOneOut(pinp, i):
    c = Comp(pinp[0][0])
    c.inp(i)
    c.run()
    return c.out()

def part1(pinp):
    return oneInOneOut(pinp, 1)

def part2(pinp):
    return oneInOneOut(pinp, 2)
            
## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    # inp = """"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, ff=lambda x:tuple(map(int, x.split(','))))

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################
