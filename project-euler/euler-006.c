/*
Sum square difference
Problem 6

The sum of the squares of the first ten natural numbers is,
1^2 + 2^2 + ... + 10^2 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 +...+10)^2 = 55^2 = 3025

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is .

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.


Answer:  25164150
*/

#include <stdio.h>

int squareOfSum(int number) {
  int i, result;
  int aux = 0;
  for (i = 1; i <= number; i++) {
    aux += i;
  }
  result = aux * aux;
  
  return result;
}

int sumOfSquare(int number) {
  int i, aux;
  int result = 0;
  
  for (i = 1; i <= number; i++) {
    aux = i * i;
    result += aux;
  }

  return result;
}

int main() {
  int num = 100;
  int squareSum = squareOfSum(num);
  int sumSquare = sumOfSquare(num);
  int result = squareSum - sumSquare;
  
  printf("%d\n", result);

  return 0;
}
