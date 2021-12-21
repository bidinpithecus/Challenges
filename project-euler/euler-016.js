/* 

2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?

*/


function sumOfDigits(num) {
  num = Array.from(String(num), Number);
  let sum = 0;

  for (let i = 0; i < num.length; i++) {
    sum += num[i];
  }
  
  return sum; 
}

let power = 1000;
let num = BigInt(2 ** power);

console.log(sumOfDigits(num));