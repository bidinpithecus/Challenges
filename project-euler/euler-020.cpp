/*
Factorial digit sum
Problem 20

n! means n × (n − 1) × ... × 3 × 2 × 1

For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800, and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

Find the sum of the digits in the number 100!
*/

#include <iostream>

using namespace std;

int sumOfDigits(long long num) {
    int sum = 0;

    while (num != 0) {
        sum += num % 10;
        num /= 10;
    }

    return sum;
}

long long factorial(int num) {
    long long result = 1;
    for (int i = num; i > 1; i--) {
        result *= i;
    }
    return result;
}

int main(void) {
    for (int i = 1; i < 20; i++) {
        long long fact = factorial(i);
        cout << "factorial(" << i << ") = " << fact << "\t sumOfDigits = " << sumOfDigits(fact)<< '\n';
    }

    return 0;
}