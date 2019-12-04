#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// int n[] is a pointer so we have to
// give the number of elements (size)
// as an arg
bool contains_repeated(int n[], int s) {
  for(int i = 0; i < s-1; i++) {
    if(n[i+1] == n[i])
      return true;
  }
  return false;
}

bool only_increases(int n[], int s) {
  for(int i = 0; i < s-1; i++) {
    if(!(n[i+1] >= n[i]))
      return false;
  }
      return true;
}

int main() {
    int total_passwords = 0;

    // Puzzle input: 108457-562041
    for (int num = 108457; num < 562041; num++) {
        // convert int 123 to array[1,2,3]
        int tmp = num;
        int n[6]; // passwords are 6 ints long
        for (int i = 5; i >=0; i--) {
            n[i] = tmp % 10;
            tmp /= 10;
        }
    
        // length of array n[]
        // ... which we already know (6) but
        // if you ever have to figure it
        // out during run time this is how
        // to do it
        int size = sizeof(n) / sizeof(n[0]); 

        if (contains_repeated(n, size) && only_increases(n, size)) {
            total_passwords++;
        }
    }

      printf("total passwords: %d\n", total_passwords);
      exit(EXIT_SUCCESS);
}

