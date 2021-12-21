/*
10001st prime
Problem 7

By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?


Answer:  104743
*/

#include <stdio.h>
#include <math.h>


int isPrime(int number) {
  int numSqrt = sqrt(number) + 1;
  int i;
  int divs = 0;

  if (number == 2) {
    return number;
  } else if (number == 1 || number % 2 == 0) {
    return 0;
  } else {
    for (i = 2; i < numSqrt; i++) {
      if (number % i == 0) {
        divs++;
        break;
      }
    }
    if (divs == 1) {
      return 0;
    } else {
      return number;
    }
  }
}

int numberOfElements(int number) {
  int num = 0, numbers = 0;
  int condition = 1;
  
  while (condition) {
    if (isPrime(num)) {
      numbers++;
    }
    if (number == numbers) {
      condition = 0;
      break;
    }
    num++;
  }
  return num;
}

int main() {
  int num = 10001;
  int result = numberOfElements(num);

  printf("%d\n", result);

  return 0;
}
