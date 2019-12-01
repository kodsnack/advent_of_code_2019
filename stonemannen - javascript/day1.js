const fs = require("fs");
var input = fs.readFileSync("input1.txt", "utf8");
input = input.split("\n");

var total = 0

input.forEach(element => {
    total += Math.floor(element/3)-2
});

console.log(total)

//part 2

total = 0;

function calculateFuel(mass){
    return Math.floor(element/3)-2
}

for(var i = 0; i < input.length; i++){
    var finished = false

    while(!finished){
        input[i] = Math.floor(input[i]/3)-2
        if(input[i] < 1){
            finished = true
        }else{
            total += input[i]
        }
    }
}
console.log(total)