const {readFileSync, promises: fsPromises} = require('fs');

function main(){
  let input = readFile("input");
  part1(input);
  part2(input);
 }

function part1(input){
  let response = input.filter(x=>x.length>0).map(x => calculateSimilarItems(x)).reduce(add, 0);
  console.log(response);
}

function part2(input){
  let filteredInput = input.filter(x=>x.length>0).map(x => makeUnique(x));
  let elfBadges = [];
  for(let i = 0; i < filteredInput.length; i+=3){
    let allThree = sortString(filteredInput[i] + filteredInput[i+1] + filteredInput[i+2]);
    let regex = /([a-zA-Z])(?=(?:.*?\1){2})/g;
    let found = allThree.match(regex);
    elfBadges.push(found[0]);
  }
  console.log(elfBadges.map(x => convertLetterToScore(x)).reduce(add,0));
}

function add(accumulator, a){
  return accumulator + a;
}

function calculateSimilarItems(item) {
  const middleIndex = Math.ceil(item.length / 2);
  const firstHalf = makeUnique(item.slice(0, middleIndex));
  const secondHalf = makeUnique(item.slice(middleIndex, item.length));
  let filtered = sortString(firstHalf.concat(secondHalf));
  let code = -1;
  for(let i=0;i<filtered.length-1; i++){
    if(filtered.charAt(i) == filtered.charAt(i+1)){
      code = convertLetterToScore(filtered.charAt(i));
      i=filtered.length;
    }
  }
  return code;
}

function makeUnique(str) {
  return String.prototype.concat(...new Set(str))
}

function sortString(text) {
  return text.split('').sort().join('');
}

function convertLetterToScore(letter){
  let code = letter.charCodeAt();

  if(code < 97){// capital
    code-=38;
  }
  else{ // lowercase
    code-=96;
  }
  return code;
}

function readFile(filename) {
  const contents = readFileSync(filename, 'utf-8');
  return  contents.split(/\r?\n/);
}

main();
