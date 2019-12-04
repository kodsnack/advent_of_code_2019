//part 1

const fs = require("fs");
var input = fs.readFileSync("input2.txt", "utf8")
input = input.split(',')
for(var i = 0; i < input.length; i += 4){
    if(input[i] == 1){
        input[input[i+3]] = Number(input[input[i+1]]) + Number(input[input[i+2]])
    }else if(input[i] == 2){
        input[input[i+3]] = Number(input[input[i+1]]) * Number(input[input[i+2]])
    }else if(input[i] == 99){
        console.log('part1: ' + input[0])
        break;
    }
}


//part 2

const ogInput = fs.readFileSync("input2.txt", "utf8").split(',');
for(var j = 0; j < 100; j++){
for(var k = 0; k < 100; k++){
    var input = ogInput.toString().split(',')
    input[1] = j
    input[2] = k
    for(var i = 0; i < input.length; i += 4){
        if(input[i] == 1){
            input[input[i+3]] = Number(input[input[i+1]]) + Number(input[input[i+2]])
        }else if(input[i] == 2){
            input[input[i+3]] = Number(input[input[i+1]]) * Number(input[input[i+2]])
        }else if(input[i] == 99){
            if(input[0] == 19690720){
                console.log("part2: " + Number(100*j+k))
            }
            //console.log(input[0], j, k)
            break;
        }
    }
}
}

