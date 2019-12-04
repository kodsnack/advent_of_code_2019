#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

int calculate_fuel(int mass) {
	return mass / 3 - 2;
}

int main() {
	FILE *fp;

	if ((fp = fopen("input.txt", "r")) == NULL ) {
		printf("File was not opened: errno %d\n", errno);
		exit(EXIT_FAILURE);
	}
 
	char line [48];
 	int total_fuel = 0;

 	while ( fgets ( line, sizeof line, fp ) != NULL )       {
 		total_fuel += calculate_fuel(atoi(line));
     }
	
	printf("%d", total_fuel);

	fclose(fp);
	
	exit(EXIT_SUCCESS);
}
