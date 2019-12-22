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

def dealWithInc(stack, inc):
    newStack = stack[:]
    for i,card in enumerate(stack):
        newStack[(i*inc)%len(stack)] = card
    return newStack

def dealWithIncC(card, inc, deckSize, offset, factor):
    return (card * inc)%deckSize, (offset * inc)%deckSize, (factor * inc)%deckSize

def dealIntoNewStack(stack):
    return list(reversed(stack))

def dealIntoNewStackC(card, deckSize, offset, factor):
    return (-card - 1)%deckSize, (-offset - 1)%deckSize, (-factor)%deckSize

def cut(stack, place):
    if place < 0:
        return stack[place:] + stack[:place]
    else:
        return stack[place:] + stack[:place] 

def cutC(card, place, deckSize, offset, factor):
    return (card - place)%deckSize, (offset - place)%deckSize, factor%deckSize

def part1(pinp):
    deck = list(range(10007))
    for i, s in enumerate(pinp):
        if s[0].startswith("deal with increment"):
            deck = dealWithInc(deck, int(s[0].split(' ')[3]))
        elif s[0].startswith("cut"):
            deck = cut(deck, int(s[0].split(' ')[1]))
        elif s[0].startswith("deal into new stack"):
            deck = dealIntoNewStack(deck)
        else:
            print("Huh")
    return deck.index(2019)

def powmod(a, b, c):
    if b == 0:
        return 1
    if b == 1:
        return a%c
    return powmod((a*a) % c, b//2, c) * powmod(a, b%2, c)

def part2(pinp):
    card = 2020
    card = 2019
    origCard = card
    deckSize = 119315717514047
    deckSize = 10007
    times = 101741582076661
    times = 1
    offset, factor = 0, 1
    for j in range(5):
        offset, factor = 0, 1
        for i, s in enumerate(pinp):
            if s[0].startswith("deal with increment"):
                card, offset, factor = dealWithIncC(card, int(s[0].split(' ')[3]), deckSize, offset, factor)
                # print(i, "dwi", card, offset, factor, 2019*factor+offset)
            elif s[0].startswith("cut"):
                card, offset, factor = cutC(card, int(s[0].split(' ')[1]), deckSize, offset, factor)
                # print(i, "cut", card, offset, factor, 2019*factor+offset)
            elif s[0].startswith("deal into new stack"):
                card, offset, factor = dealIntoNewStackC(card, deckSize, offset, factor)
                # print(i, "dins", card, offset, factor, 2019*factor+offset)
            else:
                print("Huh")
            # print(card%deckSize, (2019*factor+offset)%deckSize)
        print(j, card, offset, factor)
        factor2 = powmod(factor, j+1, deckSize)
        offset2 = offset * (j+1)
        print((origCard * factor2 + offset2)%deckSize)
        print(j, card%deckSize, offset2%deckSize, factor2%deckSize)
    factor = powmod(factor, times, deckSize)
    offset = offset * times
    return (2020 * factor + offset)%deckSize

## Start of footer boilerplate #################################################

if __name__ == "__main__":
    inp = readInput()
    
    ## Update for input specifics ##############################################
    parseInp = fileParse(inp)

    print("Input is '" + str(parseInp[:10])[:100] + 
          ('...' if len(parseInp)>10 or len(str(parseInp[:10]))>100 else '') + "'")
    print("Solution to part 1: {}".format(part1(parseInp)))
    print("Solution to part 2: {}".format(part2(parseInp)))

# End of footer boilerplate ###################################################
