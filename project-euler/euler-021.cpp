/*
Amicable numbers
Problem 21

Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).

If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.

Answer: 31626
*/

#include <iostream>
#include <math.h>

using namespace std;

bool isAmicable(int num) {
    int sumDivisors = 1;
    for (int i = 2; i < ceil(sqrt(num)); i++) {
        if (num % i == 0) {
            sumDivisors += i;
            sumDivisors += num / i;
        }
    }

    if (sumDivisors == num) {
        return false;
    }

    int sumDivisorsOfSum = 1;
    for (int i = 2; i < ceil(sqrt(sumDivisors)); i++) {
        if (sumDivisors % i == 0) {
            sumDivisorsOfSum += i;
            sumDivisorsOfSum += sumDivisors / i;
        }
    }
    return (num == sumDivisorsOfSum);
}

int main(void) {
    int sum = 0;
    int max = 10000;

    for (int i = 1; i < max; i++) {
        if (isAmicable(i)) {
            sum += i;
        }
    }

    cout << sum << '\n';

    return 0;
}