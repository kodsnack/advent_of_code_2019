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

def layers(pixels, width, height):
    size = width * height
    while len(pixels) >= size:
        yield pixels[:150]
        pixels = pixels[150:]

def part1(pinp):
    minimum0 = 150
    for layer in layers(pinp[0][0], 25, 6):
        if layer.count('0') < minimum0:
            minimum0 = layer.count('0')
            checkProduct = layer.count('1') * layer.count('2')
    return checkProduct

def part2(pinp):
    image = list('2'*(25*6))
    for layer in layers(pinp[0][0], 25, 6):
        for idx, pixel in enumerate(layer):
            if image[idx] == '2' and pixel != '2':
                image[idx] = pixel
    image = ''.join(image).replace('1', 'X').replace('0', ' ')
    return "\n"+"\n".join([image[i:i+25] for i in range(0, len(image), 25)])

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    # inp = """"""
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

## End of footer boilerplate ###################################################
