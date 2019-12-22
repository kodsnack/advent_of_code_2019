import sys

N = 119315717514047
M = 101741582076661
POS = 2020

# multiply two matrices
def matrix_mult(matA, matB):
    return ((matA[0]*matB[0] + matA[1]*matB[2]) % N,
            (matA[0]*matB[1] + matA[1]*matB[3]) % N,
            (matA[2]*matB[0] + matA[3]*matB[2]) % N,
            (matA[2]*matB[1] + matA[3]*matB[3]) % N)

# matrix mat to the exp-th power
def matrix_power(mat, exp):
    mul = mat
    ans = (1,0,0,1)
    while exp > 0:
        if exp % 2 == 1:
            ans = matrix_mult(mul, ans)
        exp //= 2
        mul = matrix_mult(mul, mul)
    return ans

# inverse of num, modulo p prime, using Fermat's little theorem
def inv_prime(num, p):
    exp = p-2
    mul = num
    ans = 1
    while exp > 0:
        if exp % 2 == 1:
            ans = (ans * mul) % p
        exp //= 2
        mul = (mul * mul) % p
    return ans

# card i always in position ai+b (mod N)
a = 1
b = 0

# apply the shuffle process once
for line in open("day22.txt"):
    x = line.strip().split()
    if x[0] == 'cut':
        # position decreased by k
        k = int(x[1])
        b = (b-k)%N
    elif x[1] == 'into':
        # position multiplied by -1
        a = (-a)%N
        b = (-b-1)%N
    elif x[1] == 'with':
        # position multiplied by k
        k = int(x[3])
        a = (a*k)%N
        b = (b*k)%N

# apply the process M times
# card i will be at the position given by the first element of the vector
#             M
#        [a b]  [i]
#        [0 1]  [1]
# which is M-th power of the 2x2 matrix {{a,b},{0,1}} multiplied by the column vector {{i},{1}}
res = matrix_power((a,b,0,1), M)

# if A and B are the top elements of the M-th power matrix,
# we need to solve for i in Ai+B = POS (mod N)
ansA, ansB = res[:2]
print((inv_prime(ansA, N)*(POS-ansB))%N)

