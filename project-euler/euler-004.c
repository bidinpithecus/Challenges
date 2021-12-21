/*
Largest palindrome product
Problem 4

A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.


Answer:  906609
*/

#include <stdio.h>

int reverseNumber(int num) {
  int rev = 0;
  int temp;
  
  while (num != 0) {
    temp = num % 10;
    rev = (rev * 10) + temp;
    num /= 10;
  }
  
  return rev;
}

int palindromeProduct(int numb1, int numb2) {
  // return 0 if not palindrome
  // return the number if it is a palindrome
  int result = numb1 * numb2;
  int reverseNum = reverseNumber(result);
  int i;

  if (result == reverseNum) {
    return result;
  } else {
    return 0;
  }
}


int main(void) {
  int i, j;
  int result, aux;
  
  for (i = 100; i < 1000; i++) {
    for (j = 100; j < 1000; j++) {
      result = palindromeProduct(i, j);
      if ((i * j) == result) {
        if (aux < result) {
          aux = result;
        }
      }
    }
  }
  printf("%d\n", aux);
  
  return 0;
}