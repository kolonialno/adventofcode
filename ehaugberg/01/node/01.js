const fs = require('fs');

const EMPTY = '';

const getSums = () => {
  const input = fs.readFileSync('input.txt', {encoding: 'utf-8'});
  const inputSplit = input.split('\n');

  const sums = inputSplit.reduce((acc, value) => {
    if (value === EMPTY) {
      return {currentSum: 0, sums: [acc.currentSum, ...acc.sums]};
    }

    return {currentSum: acc.currentSum + parseInt(value), sums: acc.sums};
  }, {currentSum: 0, sums: []})

  // only interested in the final list of sums
  return sums.sums;
}

const partOne = () => {
  return getSums().sort().reverse()[0];
}

const partTwo = () => {
  const sum = (a, b) => a + b;
  const desc = (a, b) => b - a;

  return getSums().sort(desc).slice(0, 3).reduce(sum);
}

console.log('part 1', partOne())
console.log('part 2', partTwo())