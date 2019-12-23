## Start of header boilerplate #################################################

from aocbase import readInput
import re
import random
import copy

def lineParse(s, f, fp):
    m = fp.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp, f=lineParse, ff=lambda x:x, fp=re.compile(r"^(.*)$")):
    return tuple(map(lambda x:f(x, ff, fp), inp.splitlines()))

## End of header boilerplate ###################################################

def drawMap(m, default=' ', separator='', upsideDown=True):
    keys = [(key[0], key[1]) for key in m.keys() if isinstance(key, tuple)]
    xmin = min(map(lambda x:x[0], keys))
    xmax = max(map(lambda x:x[0], keys))
    ymin = min(map(lambda x:x[1], keys))
    ymax = max(map(lambda x:x[1], keys))
    lines = []
    lines.append(separator*(xmax-xmin+1))
    if upsideDown:
        rng = range(ymin, ymax+1)
    else:
        rng = range(ymax, ymin - 1, -1)
    for y in rng:
        line = ''.join((m.get((x, y), default) for x in range(xmin, xmax+1)))
        lines.append(line)
    lines.append(separator*(xmax-xmin+1))
    return '\n'.join(lines)

class TrapException(RuntimeError):
    pass

class Memory:
    def __init__(self, comp):
        self.storage = dict()
        self.comp = comp
    def __getitem__(self, key):
        if not isinstance(key, int) or key < 0:
            self.comp.trap("Invalid memory access key {}".format(key))
        else:
            return self.storage.get(key, 0)
    def __setitem__(self, key, value):
        if not isinstance(key, int) or key < 0:
            self.comp.trap("Invalid memory access key {}".format(key))
        else:
            self.storage[key] = value
    def load(self, content, start=0):
        self.storage.update({idx+start: value for idx, value in enumerate(content)})
        return self

class Comp:
    def trap(self, msg):
        self.error.apped((self.pc, self.msg))
        self.mode = "halted"
        raise TrapException()
    def writeTarget(self, argNo):
        mode = self.mem[self.pc]//(10*10**argNo)%10
        if mode == 2:
            return self.mem[self.pc+argNo] + self.relBase
        elif mode == 0:
            return self.mem[self.pc+argNo]
        else:
            self.trap("Illegal mode for write addressing {}".format(mode))
    def readVal(self, argNo):
        mode = self.mem[self.pc]//(10*10**argNo)%10
        if mode == 1:
            return self.mem[self.pc+argNo]
        elif mode == 2:
            return self.mem[self.mem[self.pc+argNo] + self.relBase]
        elif mode == 0:
            return self.mem[self.mem[self.pc+argNo]]
        else:
            self.trap("Illegal mode for read addressing {}".format(mode))

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
        self.mem = Memory(self).load(pinp)
        self.pc = 0
        self.relBase = 0
        self.state = "ready"
        self.input = []
        self.output = []
        self.error = []
        self.counter = 0
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
        if self.outEmpty():
            self.trap("Attempt to read empty output queue")
        ret = self.output[0]
        del self.output[0]
        return ret
    def outEmpty(self):
        return len(self.output)==0
    def step(self):
        if self.halted():
            return
        # print(self.pc, self.mem[self.pc], self.mem[self.pc+1], self.mem[self.pc+2], self.mem[self.pc+3])
        instr = self.getInstr()
        if instr in self.ilist:
            self.pc = self.ilist[instr](self)
            self.counter += 1
        else:
            self.trap((self.pc, "Invalid instruction {}".format(self.mem[self.pc])))
    def run(self):
        try:
            while(self.ready()):
                self.step()
        except TrapException:
            if (len(self.error) > 0):
                for error in self.error:
                    print("{:08D}: {}".format(error[0], error[1]))
        if (len(self.error) > 0):
            for error in self.error:
                print("{:08D}: {}".format(error[0], error[1]))
    def asciiOut(self):
        l = list()
        while not self.outEmpty():
            c = self.out()
            if c > 255:
                return c
            c = chr(c)
            if c == '\n':
                return ''.join(l)
            else:
                l.append(c)
        return ''.join(l)
    def asciiIn(self, s):
        for c in s:
            self.inp(ord(c))

def testProg(main, l, run=False):
    comp  = Comp(main)
    l = '\n'.join(l) + ("\nRUN\n" if run else "\nWALK\n")
    comp.asciiIn(l)
    comp.run()
    l = list()
    while(not comp.outEmpty()):
        line = comp.asciiOut()
        if isinstance(line, int):
            return line
        l.append(line)
    print('\n'.join(l))

def testSpecimen(main, l, trace=False):
    comp  = Comp(main)
    prog = '\n'.join([' '.join(line) for line in l]) + '\nWALK\n'
    # print(prog)
    comp.asciiIn(prog)
    comp.run()
    l = list()
    won = 0
    while(not comp.outEmpty()):
        line = comp.asciiOut()
        if isinstance(line, int):
            won = line
        elif trace:
            l.append(line)
    return comp.counter, won, '\n'.join(l)

def part1(pinp):
    prog = [
        "OR A J", # No hole one step ahead
        "AND B J", # No hole one or two steps ahead
        "AND C J", # No hole one, two or three steps ahead
        "NOT J J", # Invert, jump if hole any of steps 1-3
        "AND D J", # But wait if landing in hole
    ]
    return testProg(pinp[0][0], prog)

def part2(pinp):
    prog = [
        "OR A J", # No hole one step ahead
        "AND B J", # No hole one or two steps ahead
        "AND C J", # No hole one, two or three steps ahead
        "NOT J J", # Invert, jump if hole any of steps 1-3
        "AND D J", # But wait if landing in hole
        "OR E T", # Will I be able to run on after jump?
        "OR H T", # Or can I jump directly again?
        "AND T J", # Then go ahead and jump, else wait
    ]
    return testProg(pinp[0][0], prog, run=True)

def randomOp():
    return ['NOT', 'OR', 'AND'][random.randrange(3)]

def randomArg1():
    return "ABCDTJ"[random.randrange(6)]

def randomArg2():
    return "TJ"[random.randrange(2)]

def randomRow():    
    li = []
    li.append(randomOp())
    li.append(randomArg1())
    li.append(randomArg2())
    return li

def generateRandomSpringScript():
    sc = []
    for i in range(random.randint(1, 15)):
        sc.append(randomRow())
    return sc

def startPopultation(no):
    l = list()
    for i in range(no):
        spsc = generateRandomSpringScript()
        l.append([spsc, 0])
    return l

def showTop(pop, no):
    for i, p in enumerate(pop[:no]):
        print(i+1, p[1])
        if i<3:
            print(pop[i][0])

def evaluatePopulation(pinp, pop):
    for i in range(len(pop)):
        counter, won, trace = testSpecimen(pinp[0][0], pop[i][0])
        pop[i][1] = counter - len(pop[i][0])**4

def cullPopulation(pop, no):
    pop.sort(key=lambda x:-x[1])
    d = dict()
    i = 0
    while len(pop) < no:
        prog = '\n'.join([' '.join(line) for line in pop[i][0]])
        if prog not in d:
            d[prog] = 0
        d[prog] += 1
        if d[prog] > (no // 10):
            del pop[i]
        else:
            i += 1
    pop[no:] = []

def mutateSpecimen(spec):
    i = random.randrange(5)
    newSpec = copy.deepcopy(spec)
    if i == 0: # Add row
        linePos = random.randrange(len(newSpec[0])+1)
        newSpec[0][linePos:linePos] = [randomRow()]
    if i == 1:
        idx = random.randrange(len(newSpec[0]))
        newSpec[0][idx][0] = randomOp()
    if i == 2:
        idx = random.randrange(len(newSpec[0]))
        newSpec[0][idx][1] = randomArg1()
    if i == 3:
        idx = random.randrange(len(newSpec[0]))
        newSpec[0][idx][2] = randomArg2()
    if i == 4: # Remove row
        idx = random.randrange(len(newSpec[0]))
        newSpec[0][idx:idx+1] = []
    return newSpec

def breedSpecimens(spec1, spec2):
    newSpec = copy.deepcopy(spec1)
    for i in range(len(newSpec[0])):
        if i < len(spec2[0]) and random.random() < 0.5:
            newSpec[0][i] = copy.deepcopy(spec2[0][i])
    return newSpec

def mutatePopulation(pop, prob, no):
    while len(pop) < no:
        sel = 0
        while random.random() >= prob:
            sel = (sel+1) % len(pop)
        pop.append(mutateSpecimen(pop[sel]))

def breedPopulation(pop,prob, no):
    while len(pop) < no:
        sel1 = 0
        while random.random() >= prob:
            sel1 = (sel1+1) % len(pop)
        sel2 = (sel1 + 1)  % len(pop)
        while random.random() >= prob:
            sel2 = (sel2+1) % len(pop)
        pop.append(breedSpecimens(pop[sel1], pop[sel2]))    

def part3(pinp):
    pop = startPopultation(5000)
    a = 0
    while a < 500:
        a += 1
        print("Gen ",a)
        evaluatePopulation(pinp, pop)
        cullPopulation(pop, 300)
        showTop(pop, 15)
        mutatePopulation(pop, 0.03, 650)
        breedPopulation(pop, 0.03, 1000)
    counter, won, trace = testSpecimen(pinp[0][0], pop[0][0], True)
    print(trace, won)
    print(pop[0], [0])
    

## Start of footer boilerplate #################################################

import sys
if __name__ == "__main__":
    inp = readInput()
    # inp = """"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, ff=lambda x:tuple(map(int, x.split(','))))

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))
    if "genetic" in sys.argv:
        print("Test".format(part3(parseInp)))

## End of footer boilerplate ###################################################
