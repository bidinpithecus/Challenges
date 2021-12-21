/*

Largest Prime Factor

The prime factors of 13195 are 5, 7, 13 and 29.
What is the largest prime factor of the number 600851475143 ?


Answer:  6857
*/

#include <stdio.h>
#include <math.h>
#include <time.h>

int isPrime(int number) {
  int zeros = 0;
  int numberSqrt = ceil(sqrt(number));

  if (number == 1) {
    return 0;

  } else if (number == 2) {
    return 1;
      
  } else if (number % 2 == 0) {
    return 0;
  
  } else {
    for (int i = 1; i <= numberSqrt; i++) {
      if (number % i == 0) {
        zeros += 1;
        break;
      }
      if(zeros != 1) {
        return 0;
      }
    }
    return 1;  
  }
}

long long primeFactors(long long x) {
  int higher;
  for (long long i = 2; i < ceil(sqrt(x)); i++) {
    if (x % i == 0) {
      if (isPrime(i) == 1) {
        higher = i;
      }
    }
  }
  return higher;    
}

int main(void) {
  clock_t time = clock();
  int result = primeFactors(600851475143);

  time = clock() - time;

  printf("result: %d found in %f seconds\n", result, ((float)time)/CLOCKS_PER_SEC);

  return 0;

}
