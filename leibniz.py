#!/usr/bin/env python3


def step_reverse_sign(a, b):
    sign = lambda x: 0 if x == 0 else -1 if x < 0 else 1
    return sign(-a) * (sign(a) * a + b)


def piCalc(tol):
    curr, prev, d, it = 4, 0, 1, 0
    while abs(curr - prev) > tol:
        it += 1
        d = step_reverse_sign(d, 2)
        curr, prev = curr + 4 / d, curr

    return curr, it

print(piCalc(0.001))
