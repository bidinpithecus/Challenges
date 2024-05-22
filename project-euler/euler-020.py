'''
Factorial digit sum
Problem 20

n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800, and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
'''

def fact(n):
    f = 1
    for i in range(2, n + 1):
        f *= i
    return f

def sum_digits_of_num(n):
    s = 0
    for c in str(n):
        s += int(c)
    return s

n = 100
n_fact = fact(n)
n_fact_sum_digits = sum_digits_of_num(n_fact)
print(str(n) + "! is " + str(n_fact) + ".\nThe sum of its digits is " + str(n_fact_sum_digits))
