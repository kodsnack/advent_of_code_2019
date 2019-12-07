import sys


def _get_param_values(memory, start, n, modes):
    if len(modes) < n:
        modes = modes + [0] * (n - len(modes))
    vals = []
    for i in range(start, start + n):
        val = memory[i]
        if modes[i - start] == 0:  # indirect mode
            vals.append(memory[val])
        elif modes[i - start] == 1:  # direct mode
            vals.append(val)
        else:
            raise ValueError(f'Unkown mode {modes[i-start]}')
    return vals


def _opcode1(memory, ip, modes):
    op1, op2 = _get_param_values(memory, ip + 1, 2, modes)
    res = memory[ip + 3]
    # print(f'memory[{res}] = {op1} + {op2}')
    memory[res] = op1 + op2
    return memory, ip + 4


def _opcode2(memory, ip, modes):
    op1, op2 = _get_param_values(memory, ip + 1, 2, modes)
    res = memory[ip + 3]
    # print(f'memory[{res}] = {op1} * {op2}')
    memory[res] = op1 * op2
    return memory, ip + 4


def _opcode3(memory, ip, modes, inputs):
    _, op1 = memory[ip : ip + 2]
    try:
        memory[op1] = inputs.get()
    except AttributeError:
        # Fall back to assuming a regular list
        memory[op1] = inputs.pop(0)
    # print(f'memory[{op1}] = {memory[op1]}')
    return memory, ip + 2


def _opcode4(memory, ip, modes, outputs):
    _, op1 = memory[ip : ip + 2]
    # if memory[op1] != 0:
    #     breakpoint()
    try:
        outputs.put(memory[op1])
    except AttributeError:
        # Fall back to assuming a regular list
        outputs.append(memory[op1])
    return memory, ip + 2


def _opcode5(memory, ip, modes):
    op1, op2 = _get_param_values(memory, ip + 1, 2, modes)
    if op1 != 0:
        # print(f'jump to {op2}')
        return memory, op2
    return memory, ip + 3


def _opcode6(memory, ip, modes):
    op1, op2 = _get_param_values(memory, ip + 1, 2, modes)
    if op1 == 0:
        # print(f'jump to {op2}')
        return memory, op2
    return memory, ip + 3


def _opcode7(memory, ip, modes):
    op1, op2 = _get_param_values(memory, ip + 1, 2, modes)
    res = memory[ip + 3]
    memory[res] = int(op1 < op2)
    return memory, ip + 4


def _opcode8(memory, ip, modes):
    op1, op2 = _get_param_values(memory, ip + 1, 2, modes)
    res = memory[ip + 3]
    memory[res] = int(op1 == op2)
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


def _parse_parameter_mode(opcode):
    s = str(opcode)
    if len(s) == 1:
        return []
    return list(reversed([int(i) for i in s[:-2]]))


def _parse_opcode(opcode):
    if opcode > 10:
        opcode %= 100
    try:
        return opcodes[opcode]
    except KeyError:
        raise ValueError(f"Unknown opcode {opcode}")


def run(memory, ip=0, inputs=None, outputs=None):
    inputs = inputs or []
    outputs = outputs or []
    while memory[ip] != 99:
        modes = _parse_parameter_mode(memory[ip])
        func = _parse_opcode(memory[ip])

        if func == _opcode3:  # special case input
            memory, ip = func(memory, ip, modes, inputs)
        elif func == _opcode4:  # special case output
            memory, ip = func(memory, ip, modes, outputs)
        else:
            memory, ip = func(memory, ip, modes)

    return memory, ip, inputs, outputs


def test_parse_parameter_mode():
    assert _parse_parameter_mode(1002) == [0, 1]
    assert _parse_parameter_mode(11003) == [0, 1, 1]
    assert _parse_parameter_mode(3) == []
    assert _parse_parameter_mode(101) == [1]


def test_get_param_values():
    memory = [1002, 4, 3, 4, 33]
    modes = _parse_parameter_mode(memory[0])
    assert _get_param_values(memory, 1, 2, modes) == [33, 3]

    memory = [101, -1311, 4, 4, 4]
    modes = _parse_parameter_mode(memory[0])
    assert _get_param_values(memory, 1, 2, modes) == [-1311, 4]


def test_get_param_values_no_mode():
    memory = [2, 4, 3, 4, 33]
    modes = _parse_parameter_mode(memory[0])
    assert _get_param_values(memory, 1, 2, modes) == [33, 4]
