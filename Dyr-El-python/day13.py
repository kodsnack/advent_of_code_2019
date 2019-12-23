## Start of header boilerplate #################################################

from aocbase import readInput
import re
import time

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

def part1(pinp):
    c = Comp(pinp[0][0])
    c.run()
    tot = 0
    while not c.outEmpty():
        x = c.out()
        y = c.out()
        t = c.out()
        if t == 2:
            tot += 1
    return tot

def initScr(cs):
    if cs:
        cs.nodelay(True)
        curses.curs_set(0)
    return dict()

def updatePos(cs, screen, x, y, c):
    screen[x, y] = c
    if cs:
        cs.addstr(y, x, c)

def updateScore(cs, screen, score):
    screen["score"] = score
    if cs:
        cs.addstr(1, 45, "Score: {}".format(score))

lastTime = None
def updateScreen(cs):
    global lastTime
    if cs:
        c = cs.getch()
        now = time.time()
        if lastTime:
            time.sleep(max(0.1 - (now - lastTime), 0))
        lastTime = now
        if 0 < c < 256:
            return chr(c)
    return ""

def runPart(c, scr):
    screen = dict()
    screen = initScr(scr)
    xmin,xmax,ymin,ymax = 0, 0, 0, 0
    nofblocks = 0
    paddlex = 0
    ballx = 0
    while c.state != "halted":
        c.run()
        if updateScreen(scr)=='Q':
            break
        while not c.outEmpty():
            x = c.out()
            y = c.out()
            t = c.out()
            if x == -1 and y == 0:
                updateScore(scr, screen, t)
                continue
            elif t == 0:
                if screen.get((x, y), ' ')  == '#':
                    nofblocks -= 1
                updatePos(scr, screen, x, y, ' ')
            elif t == 1:
                updatePos(scr, screen, x, y, '|')
            elif t == 2:
                updatePos(scr, screen, x, y, '#')
                nofblocks += 1
            elif t == 3:
                paddlex = x
                updatePos(scr, screen, x, y, '=')
            elif t == 4:
                ballx = x
                updatePos(scr, screen, x, y, 'O')
            xmin = min(x, xmin)
            xmax = max(x, xmax)
            ymin = min(y, ymin)
            ymax = max(y, ymax)
        if nofblocks == 0:
            return screen["score"]
        if ballx < paddlex:
            c.inp(-1)
        elif ballx > paddlex:
            c.inp(1)
        else:
            c.inp(0)
    return screen["score"]

def part2(pinp, sc):
    c = Comp(pinp[0][0])
    c.mem[0] = 2
    return runPart(c, sc)

## Start of footer boilerplate #################################################

def main(stdscreen, ):
    inp = readInput()
    # inp = """"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp, ff=lambda x:tuple(map(int, x.split(','))))

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")

    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp, stdscreen)))

import sys
if __name__ == "__main__":
    if "gui" in sys.argv:
        import curses
        curses.wrapper(main)
    else:
        main(None)

## End of footer boilerplate ###################################################
