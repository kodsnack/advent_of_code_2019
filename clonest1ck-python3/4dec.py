range_lower = 165432
range_upper = 707912

# Part 1
numbers = []

for first in range(1,10):
    for second in range(first, 10):
        for third in range(second, 10):
            for fourth in range(third, 10):
                for fifth in range(fourth, 10):
                    for sixth in range(fifth, 10):
                        if first == second or second == third or third == fourth or fourth == fifth or fifth == sixth:
                            number = int("%d%d%d%d%d%d" % (first, second, third, fourth, fifth, sixth))
                            if number >= range_lower and number <= range_upper:
                                numbers.append(number)

print("Part 1 %d" % len(numbers))

# Part 2
numbers_2 = []
for number in numbers:
    digits = []
    last_digit = ""
    group = []
    for s in str(number):
        if s != last_digit:
            digits.append(group)
            group = []
        group.append(s)
        last_digit = s
    digits.append(group)
    digits = list(map(len, digits))
    if 2 in digits:
        numbers_2.append(number)

print("Part 2 %d" % len(numbers_2))
