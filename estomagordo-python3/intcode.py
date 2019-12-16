from collections import defaultdict

class Computer:
    def __init__(self, program, input, startval=None):
        self.input = input
        self.program = defaultdict(int)
        self.p = 0
        self.relbase = 0
        self.steps = 0

        for i, v in enumerate(program):
            self.program[i] = v

        if startval:
            self.program[0] = startval

    def step(self):
        self.steps += 1
        amode = (self.program[self.p] % 1000) // 100
        bmode = (self.program[self.p] % 10000) // 1000
        cmode = (self.program[self.p] % 100000) // 10000

        a = self.program[self.p + 1] if amode == 1 else self.program[self.program[self.p + 1]] if amode == 0 else self.program[self.program[self.p + 1] + self.relbase]
        b = self.program[self.p + 2] if bmode == 1 else self.program[self.program[self.p + 2]] if bmode == 0 else self.program[self.program[self.p + 2] + self.relbase]
        c = self.program[self.p + 3] if cmode == 1 else self.program[self.program[self.p + 3]] if cmode == 0 else self.program[self.program[self.p + 3] + self.relbase]

        instruction = self.program[self.p]

        if instruction % 100 == 99:
            return -1, self.program[0]
        elif instruction % 100 == 1:
            if cmode == 0:
                self.program[self.program[self.p + 3]] = a + b
            else:
                self.program[self.program[self.p + 3] + self.relbase] = a + b
            self.p += 4
        elif instruction % 100 == 2:
            if cmode == 0:
                self.program[self.program[self.p + 3]] = a * b
            else:
                self.program[self.program[self.p + 3] + self.relbase] = a * b
            self.p += 4
        elif instruction % 100 == 3:
            if amode == 0:
                self.program[self.program[self.p + 1]] = self.input
            else:
                self.program[self.program[self.p + 1] + self.relbase] = self.input
            self.p += 2
        elif instruction % 100 == 4:
            self.p += 2
            return 1, a                
        elif instruction % 100 == 5:
            if a != 0:
                self.p = b
            else:
                self.p += 3
        elif instruction % 100 == 6:
            if a == 0:
                self.p = b
            else:
                self.p += 3
        elif instruction % 100 == 7:
            cc = 1 if a < b else 0
            if cmode == 0:
                self.program[self.program[self.p + 3]] = cc
            else:
                self.program[self.program[self.p + 3] + self.relbase] = cc
            self.p += 4
        elif instruction % 100 == 8:
            cc = 1 if a == b else 0
            if cmode == 0:
                self.program[self.program[self.p + 3]] = cc
            else:
                self.program[self.program[self.p + 3] + self.relbase] = cc
            self.p += 4
        elif instruction % 100 == 9:
            self.relbase += a
            self.p += 2
        else:
            print('uh oh', self.program[self.p])

        return 0, instruction % 100

    def get_output(self):
        retcode, retval = 0, 0
        
        while retcode == 0:
            retcode, retval = self.step()

        return retcode, retval

    def set_input(self, inp):
        self.input = inp