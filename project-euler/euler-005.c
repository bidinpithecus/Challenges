/*
Smallest multiple
Problem 5

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?


Answer:  232792560
*/

#include <stdio.h>

int multiples(int divisors) {
  // this function will return the smallest number evenly divisible by 1 to 'divisors'
  int flags = 0;
  int i;
  int num = divisors;
  
  while (flags != divisors) {
    flags = 0;
    for (i = 1; i <= divisors; i++) {
      if (num % i == 0) {
        flags++;
      } else {
        num++;
        break;
      }
    }
  }
  return num;
}


int main() {
  int divs = 20;
  int result = multiples(divs);
  
  printf("%d\n", result);

  return 0;
}
