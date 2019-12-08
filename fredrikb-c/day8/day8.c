#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

int main() {
    int width = 25;
    int height = 6;
    int layer_size = width * height;
    int result = 0;
    int zeros = 0,ones = 0,twos = 0;
    int minzeros = layer_size;
    
    FILE *fp;
    
    if((fp = fopen("input.txt", "r")) == NULL) {
        printf("could not open file: errno %d\n", errno);
        exit(EXIT_FAILURE);
    }
    
    char line[15100]; // ugly hardcoded size
    fgets ( line, sizeof line, fp );
    fclose(fp);
    
    int i = 0;
    do {
        // count the 0,1,2
        if (line[i] == '0') zeros++;
        if (line[i] == '1') ones++;
        if (line[i] == '2') twos++;
        
        // if we've read a layer worth of chars...
        if (i % layer_size == 0 && i != 0) {
            // do we have fewer zeros than before?
            if (zeros < minzeros) {
                minzeros = zeros; // new minimum amount of zeros in layer
                result = ones * twos; // calculate the current result
            }
            // reset the counters...
            zeros = 0;
            ones = 0;
            twos = 0;
        }
        // and read the next layer from the array
        i++;
    } while (line[i] != '\0'); // until we've processed everything
    
    printf("Result: %d \n", result);
    exit(EXIT_SUCCESS);
}
