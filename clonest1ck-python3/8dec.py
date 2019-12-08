def equalZero(x):
    return x == 0

def equalOne(x):
    return x == 1

def equalTwo(x):
    return x == 2

def countZerosMultiplyOnesAndTwos(data):
    zeros = len(list(filter(equalZero, data)))
    ones = len(list(filter(equalOne, data)))
    twos = len(list(filter(equalTwo, data)))

    return (zeros, ones * twos)

def getFirst(data):
    return data[0]

width = 25
height = 6
pixels_per_layer = width * height
f = open("8dec-input", 'r')
data = map(int, list(f.read())[:-1])
f.close()

layers = []
layer_counter = 0
current_layer = 0

for pixel in data:
    if layer_counter == pixels_per_layer:
        layer_counter = 0
        current_layer += 1
    if len(layers) == current_layer:
        layers.append([])

    layers[current_layer].append(pixel)
    layer_counter += 1

zero_one_two = list(map(countZerosMultiplyOnesAndTwos, layers))
zero_one_two.sort(key=getFirst)
print("Part 1: %d" % zero_one_two[0][1])

final_image = [2 for m in range(pixels_per_layer)]
for layer in layers:
    for i in range(pixels_per_layer):
        if final_image[i] == 2:
            if layer[i] == 0:
                final_image[i] = " "
            elif layer[i] == 1:
                final_image[i] = "#"

print("Part 2:")
for i in range(0, pixels_per_layer, width):
    print("".join(final_image[i:i+width-1]))

