## Start of header boilerplate #################################################

from aocbase import readInput
import re

def lineParse(s, f, fp):
    m = fp.match(s)
    if m==None:
        raise s
    return tuple(map(f, m.groups()))

def fileParse(inp, f=lineParse, ff=lambda x:x, fp=re.compile(r"^(.*)$")):
    return tuple(map(lambda x:f(x, ff, fp), inp.splitlines()))

## End of header boilerplate ###################################################

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
            self.trap((self.pc, "Invalid instruction {}".format(self.mem[self.pc])))
    def run(self):
        try:
            while(self.ready()):
                self.step()
        except TrapException:
            if (len(self.error) > 0):
                for error in self.error:
                    print("{:08D}: {}".format(error[0], error[1]))

def load(pinp):
    return Comp(pinp)

def move(c, dir):
    c.inp({"N":1,"S":2,"W":3,"E":4}[dir])
    c.run()
    if c.outEmpty():
        print("Error")
    else:
        return c.out()

def drawFloor(m):
    xmin = min(map(lambda x:x[0], m.keys()))
    xmax = max(map(lambda x:x[0], m.keys()))
    ymin = min(map(lambda x:x[1], m.keys()))
    ymax = max(map(lambda x:x[1], m.keys()))
    print("="*(xmax-xmin+1))
    for y in range(ymax, ymin-1, -1):
        for x in range(xmin, xmax+1):
            print(m.get((x, y), '.'), end='')
        print()

def mapFloor(c):
    x, y = 0, 0
    m = {(x, y):"X"}
    d = "N"
    notimes = 0
    while True:
        r = move(c, d)
        if r == 0:
            if d == "N":
                m[x, y+1] = "#"
                d = "E"
            elif d == "E":
                m[x + 1, y] = "#"
                d = "S"
            elif d == "S":
                m[x, y-1] = "#"
                d = "W"
            else:
                m[x-1, y] = "#"
                d = "N"
        elif r == 1:
            if d == "N":
                y += 1
                d = "W"
            elif d == "E":
                x += 1
                d = "N"
            elif d == "S":
                y -= 1
                d = "E"
            else:
                x -= 1
                d = "S"
            if (x, y) not in m:
                m[x, y] = " "
            elif m[x, y] in "X":
                notimes += 1
                if notimes == 4:
                    return m
                
        else:
            if d == "N":
                y += 1
                m[x, y] = "O"
                d = "W"
            elif d == "E":
                x += 1
                m[x, y] = "O"
                d = "N"
            elif d == "S":
                y -= 1
                m[x, y] = "O"
                d = "E"
            else:
                x -= 1
                m[x, y] = "O"
                d = "S"

def colorMap(m, x, y):
    pos = [(x, y)]
    m2 = {(x,y):0}
    while len(pos)>0:
        x, y = pos[0]
        if m[x, y] == "O":
            ox, oy = x, y
        pos = pos[1:]
        for dx, dy in [(0, 1), (1, 0), (-1, 0), (0, -1)]:
            nx, ny = x+dx, y+dy
            if m[nx, ny] != "#" and (nx, ny) not in m2:
                m2[nx, ny] = m2[x, y] + 1
                pos.append((nx, ny))
    return m2, ox, oy

def part1(pinp):
    c = Comp(pinp[0][0])
    m = mapFloor(c)
    cm, x, y = colorMap(m, 0, 0)
    return cm[x, y]

def part2(pinp):
    c = Comp(pinp[0][0])
    m = mapFloor(c)
    cm, x, y = colorMap(m, 0, 0)
    cm, x, y = colorMap(m, x, y)
    return max(cm.values())

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
