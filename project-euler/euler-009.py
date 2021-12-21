'''
Special Pythagorean triplet
Problem 9

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

a^2 + b^2 = c^2
For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.


Answer:  31875000
'''

from time import time
from math import pow
start_time = time()


def function(num1, num2, num3):
  if (pow(num1, 2) + pow(num2, 2) == pow(num3, 2)):
    return (num1 * num2 * num3)
  else:
    return 0

a = b = c = 0
number = 1000


for a in range(int(number / 3)):
  for b in range(int(number / 2)):
    c = number - a - b
    if (function(a, b, c)):
      answer = function(a, b, c)
      break

print(answer)
print("{:.2f} seconds".format((time() - start_time)))