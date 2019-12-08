import strutils

iterator pieces(str: string, size: int): string =
    var idx = 0
    while idx < str.len:
        yield str.substr(idx, idx + size - 1)
        idx.inc(size)

proc checkProduct*(image: string, width, height: int): int =
    let
        imageSize = width * height
    var
        minCount0 = imageSize
    for layer in pieces(image.strip(), imageSize):
        let
            no0 = layer.count('0')
        if no0 < minCount0:
            minCount0 = no0
            result = layer.count('1')*layer.count('2')

proc drawImage*(image: string, width, height: int): string =
    let imageSize = width * height
    var resultImage = '2'.repeat(imageSize)
    for layer in pieces(image, imageSize):
        for idx, pixel in layer.pairs:
            if resultImage[idx] == '2' and pixel != '2':
                resultImage[idx] = pixel
    resultImage = resultImage.replace('1', 'X').replace('0', ' ')
    for img in pieces(resultImage, width):
        result = result & '\n' & img
