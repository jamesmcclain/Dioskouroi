#!/usr/bin/env python

import sys
import math


def is_prime(n):
    if (n % 2 == 0):
        return False

    for i in range(3,int(math.ceil(math.sqrt(n)))+1,2):
        if (n % i == 0):
            return False

    return True

if __name__ == "__main__":
    print(2)
    for p in range(3,int(sys.argv[1])+1):
        if (is_prime(p)):
            print(p)

