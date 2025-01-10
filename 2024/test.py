# Decompiled program rewritten as a human-readable Python function


import re


def optimized_program(a):
    output = []
    while a != 0:
        b = ((a % 8) ^ 1) ^ (a // 2 ** ((a % 8) ^ 1)) ^ 6
        output.append(b % 8)
        a //= 8
    return output


# # Inputs
# a = 18427963
#
# # Run the optimized program
# result = optimized_program(a)
# print(result)

# Part 2: [5,3,6,6,0,2,7,4,3,4,6,11,2,2,4,7]
# target: [2, 4, 1, 1, 7, 5, 0, 3, 4, 3, 1, 6, 5, 5, 3, 0]


def decompiled_program(a, b, c):
    # Step-by-step execution of the program
    output = []
    while a != 0:
        # Restart the program loop
        b = a % 8
        b ^= 1
        c = a // 2**b
        a //= 2**3
        b ^= c
        b ^= 6
        output.append(b % 8)

    return output


# Inputs
a = 18427963
b = 0
c = 0

# Run the decompiled program
result = decompiled_program(a, b, c)

print(result)
print(optimized_program(a))
