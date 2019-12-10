import sys


def _get_modes(modes, n):
    if len(modes) < n:
        return modes + [0] * (n - len(modes))
    return modes


def _get_param_values(memory, start, n, modes, rel_base):
    vals = []
    for i in range(start, start + n):
        val = memory[i]
        if modes[i - start] == 0:  # indirect mode
            vals.append(memory[val])
        elif modes[i - start] == 1:  # direct mode
            vals.append(val)
        elif modes[i - start] == 2:  # relative mode
            vals.append(memory[val + rel_base])
        else:
            raise ValueError(f'Unkown mode {modes[i-start]}')
    return vals


def _write_output(memory, mode, out_addr, result, rel_base):
    if mode == 2:
        memory[out_addr + rel_base] = result
    else:
        memory[out_addr] = result


def _opcode1(memory, ip, modes, rel_base):
    modes = _get_modes(modes, 3)
    op1, op2 = _get_param_values(memory, ip + 1, 2, modes, rel_base)
    res = memory[ip + 3]
    # print(f'memory[{res}] = {op1} + {op2}')
    _write_output(memory, modes[-1], res, op1 + op2, rel_base)
    return memory, ip + 4


def _opcode2(memory, ip, modes, rel_base):
    modes = _get_modes(modes, 3)
    op1, op2 = _get_param_values(memory, ip + 1, 2, modes, rel_base)
    res = memory[ip + 3]
    # print(f'memory[{res}] = {op1} * {op2}')
    _write_output(memory, modes[-1], res, op1 * op2, rel_base)
    return memory, ip + 4


def _opcode3(memory, ip, modes, inputs, rel_base):
    mode = _get_modes(modes, 1)[0]
    # fmt: off
    _, op1 = memory[ip:ip + 2]
    # fmt: on
    if mode == 0:
        ind = op1
    elif mode == 2:
        ind = op1 + rel_base
    try:
        memory[ind] = inputs.get()
    except AttributeError:
        # Fall back to assuming a regular list
        memory[ind] = inputs.pop(0)
    # print(f'memory[{op1}] = {memory[op1]}')
    return memory, ip + 2


def _opcode4(memory, ip, modes, outputs, rel_base):
    mode = _get_modes(modes, 1)[0]
    # fmt: off
    _, op1 = memory[ip:ip + 2]
    # fmt: on
    # if memory[op1] != 0:
    #     breakpoint()
    if mode == 0:
        res = memory[op1]
    elif mode == 1:
        res = op1
    elif mode == 2:
        res = memory[op1 + rel_base]
    else:
        raise ValueError(f'Unknown mode {mode}')
    try:
        outputs.put(res)
    except AttributeError:
        # Fall back to assuming a regular list
        outputs.append(res)
    return memory, ip + 2


def _opcode5(memory, ip, modes, rel_base):
    modes = _get_modes(modes, 2)
    op1, op2 = _get_param_values(memory, ip + 1, 2, modes, rel_base)
    if op1 != 0:
        # print(f'jump to {op2}')
        return memory, op2
    return memory, ip + 3


def _opcode6(memory, ip, modes, rel_base):
    modes = _get_modes(modes, 2)
    op1, op2 = _get_param_values(memory, ip + 1, 2, modes, rel_base)
    if op1 == 0:
        # print(f'jump to {op2}')
        return memory, op2
    return memory, ip + 3


def _opcode7(memory, ip, modes, rel_base):
    modes = _get_modes(modes, 3)
    op1, op2 = _get_param_values(memory, ip + 1, 2, modes, rel_base)
    res = memory[ip + 3]
    _write_output(memory, modes[-1], res, int(op1 < op2), rel_base)
    return memory, ip + 4


def _opcode8(memory, ip, modes, rel_base):
    modes = _get_modes(modes, 3)
    op1, op2 = _get_param_values(memory, ip + 1, 2, modes, rel_base)
    res = memory[ip + 3]
    _write_output(memory, modes[-1], res, int(op1 == op2), rel_base)
    return memory, ip + 4


def _opcode9(memory, ip, modes, rel_base):
    modes = _get_modes(modes, 1)
    op1 = _get_param_values(memory, ip + 1, 1, modes, rel_base)[0]
    return rel_base + op1, ip + 2


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


def run(memory, ip=0, inputs=None, outputs=None, rel_base=0):
    inputs = inputs or []
    outputs = outputs or []
    while memory[ip] != 99:
        modes = _parse_parameter_mode(memory[ip])
        func = _parse_opcode(memory[ip])

        try:
            if func == _opcode3:  # special case input
                memory, ip = func(memory, ip, modes, inputs, rel_base)
            elif func == _opcode4:  # special case output
                memory, ip = func(memory, ip, modes, outputs, rel_base)
            elif func == _opcode9:  # special case relative base change
                rel_base, ip = func(memory, ip, modes, rel_base)
            else:
                memory, ip = func(memory, ip, modes, rel_base)
        except IndexError:
            memory = memory + [0] * len(memory)

    return memory, ip, inputs, outputs


def test_parse_parameter_mode():
    assert _parse_parameter_mode(1002) == [0, 1]
    assert _parse_parameter_mode(11003) == [0, 1, 1]
    assert _parse_parameter_mode(3) == []
    assert _parse_parameter_mode(101) == [1]


def test_get_modes():
    memory = [1002, 4, 3, 4, 33]
    modes = _parse_parameter_mode(memory[0])
    modes = _get_modes(modes, 3)
    assert modes == [0, 1, 0]

    memory = [109, 19]
    modes = _parse_parameter_mode(memory[0])
    modes = _get_modes(modes, 1)
    assert modes == [1]

    memory = [204, -34]
    modes = _parse_parameter_mode(memory[0])
    modes = _get_modes(modes, 1)
    assert modes == [2]


def test_get_param_values():
    memory = [1002, 4, 3, 4, 33]
    modes = _get_modes(_parse_parameter_mode(memory[0]), 3)
    assert _get_param_values(memory, 1, 2, modes, 0) == [33, 3]

    memory = [101, -1311, 4, 4, 4]
    modes = _get_modes(_parse_parameter_mode(memory[0]), 3)
    assert _get_param_values(memory, 1, 2, modes, 0) == [-1311, 4]


def test_get_param_values_no_mode():
    memory = [2, 4, 3, 4, 33]
    modes = _get_modes(_parse_parameter_mode(memory[0]), 3)
    assert _get_param_values(memory, 1, 2, modes, 0) == [33, 4]


def test_mod_rel_base():
    memory = [109, 19]
    rel_base, _ = _opcode9(memory, 0, [1], 2000)
    assert rel_base == 2019

    memory = [204, -34, 99] + [0] * 1985
    memory[1985] = 12345
    _, _, _, outputs = run(memory, rel_base=2019)
    assert outputs[0] == 12345
