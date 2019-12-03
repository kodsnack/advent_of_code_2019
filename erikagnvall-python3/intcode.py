import sys


def _opcode1(memory, ip):
    _, op1, op2, res = memory[ip:ip + 4]
    memory[res] = memory[op1] + memory[op2]
    return memory, ip + 4


def _opcode2(memory, ip):
    _, op1, op2, res = memory[ip:ip + 4]
    memory[res] = memory[op1] * memory[op2]
    return memory, ip + 4


mod = sys.modules[__name__]
opcodes = {}
for name in dir(mod):
    attr = getattr(mod, name)
    if not callable(attr):
        continue

    if name.startswith('_opcode'):
        n = int(name.replace('_opcode', ''))
        opcodes[n] = attr


def _parse_opcode(opcode):
    try:
        return opcodes[opcode]
    except KeyError:
        raise ValueError(f"Unknown opcode {opcode}")


def run(memory, ip=0):
    while memory[ip] != 99:
        func = _parse_opcode(memory[ip])
        memory, ip = func(memory, ip)
    return memory, ip
