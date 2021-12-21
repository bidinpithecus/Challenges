/*
Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down,
there are exactly 6 routes to the bottom right corner.

How many such routes are there through a 20×20 grid?
*/

function factorial(number) {
  if (number == 1) {
    return 1;
  }
  return number * factorial(number - 1);
}

function centralPascal(number) {
  let num1 = factorial(2 * number);
  let num2 = factorial(number) * factorial(number);
  
  return num1 / num2;
}

let num = 20;
let result = centralPascal(num);

console.log(result);
