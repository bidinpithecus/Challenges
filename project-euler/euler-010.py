'''
Summation of primes
Problem 10

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.


Answer:  142913828922
'''


## Summation of primes - Euler #10

from time import time
from math import sqrt
start_time = time()

def isPrime(number):
  if (number == 1):
    return 0
  else:
    numSqrt = int(sqrt(number))
    zeros = 0
    for i in range(2, numSqrt + 1):
      if (number % i == 0):
        zeros += 1
        break
    if (zeros):
      return 0
    else:
      return number


answer = 2 #starting in 2 because will not consider 2 in the sequence of primes
limit = 2000000
for i in range(1, limit + 1, 2):
  if (isPrime(i)):
    answer += i

print(answer)
print("{:.2f} seconds".format((time() - start_time)))