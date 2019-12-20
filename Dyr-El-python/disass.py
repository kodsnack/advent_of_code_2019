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
        self.error.append((self.pc, msg))
        self.mode = "halted"
        raise TrapException()
    def writeTarget(self, argNo):
        mode = self.mem[self.pc]//(10*10**argNo)%10
        if mode == 2:
            return "b[{}]".format(self.mem[self.pc+argNo])
        elif mode == 0:
            return "m[{}]".format(self.mem[self.pc+argNo])
        else:
            self.trap("Illegal mode for write addressing {}".format(mode))
    def readVal(self, argNo):
        mode = self.mem[self.pc]//(10*10**argNo)%10
        if mode == 1:
            return "#{}".format(self.mem[self.pc+argNo])
        elif mode == 2:
            return "b[{}]".format(self.mem[self.pc+argNo])
        elif mode == 0:
            return "m[{}]".format(self.mem[self.pc+argNo])
        else:
            self.trap("Illegal mode for read addressing {}".format(mode))

    def addI(self):
        target = self.writeTarget(3)
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        return self.pc+4, "ADD {} + {} -> {}".format(arg1, arg2, target)

    def mulI(self):
        target = self.writeTarget(3)
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        return self.pc+4, "MUL {} * {} -> {}".format(arg1, arg2, target)

    def inpI(self):
        target = self.writeTarget(1)
        return self.pc+2, "INP => {}".format(target)

    def outI(self):
        arg1 = self.readVal(1)
        return self.pc + 2, "OUT {} =>".format(arg1)

    def jitI(self):
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        return self.pc+3, "JIT {} ? {} -> PC".format(arg1, arg2)

    def jifI(self):
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        return self.pc+3, "JIF ! {} ? {} -> PC".format(arg1, arg2)

    def ltI(self):
        target = self.writeTarget(3)
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        return self.pc+4, "LT {} < {} ? -> {}".format(arg1, arg2, target)

    def eqI(self):
        target = self.writeTarget(3)
        arg1 = self.readVal(1)
        arg2 = self.readVal(2)
        return self.pc+4, "EQ {} == {} ? -> {}".format(arg1, arg2, target)

    def aBase(self):
        arg1 = self.readVal(1)
        return self.pc+2, "BAS {} -> b".format(arg1)

    def halt(self):
        self.state = "halted"
        return self.pc+1, "HLT"

    def getInstr(self):
        return self.mem[self.pc] % 100

    ilist = {1:addI, 2:mulI, 3:inpI, 4:outI, 5:jitI, 6:jifI, 7:ltI, 8:eqI,
        9: aBase, 99:halt}

    def __init__(self, pinp):
        self.mem = Memory(self).load(pinp)
        self.pc = 0
        self.relBase = 0
        self.state = "ready"
        self.error = []
    def ready(self):
        return self.state == "ready"
    def waiting(self):
        return self.state == "waiting"
    def halted(self):
        return self.state == "halted"
    def step(self):
        instr = self.getInstr()
        if instr in self.ilist:
            prePc = self.pc
            mem4 = ' '.join(("{:03d}".format(self.mem[i]) for i in range(prePc, prePc+4)))
            self.pc, txt = self.ilist[instr](self)
            return "{:04d}:{} | {}".format(prePc, mem4, txt)
        else:
            self.trap((self.pc, "Invalid instruction {}".format(self.mem[self.pc])))
    def run(self):
        try:
            while(self.ready()):
                print(self.step())
        except TrapException:
            if (len(self.error) > 0):
                for error in self.error:
                    print("{:04d}: {}".format(error[0], error[1]))
with open("day05.txt") as inpFile:
    inp = inpFile.read()
inData = [int(s) for s in inp.strip().split(",")]
comp = Comp(inData)
comp.run()