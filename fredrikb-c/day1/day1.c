#include <stdio.h>
#include <stdlib.h>

int calculate_fuel(int mass) {
	return mass / 3 - 2;
}

int main() {
	FILE *fp;
	errno_t err;

	if ((err = fopen_s(&fp, "input.txt", "r")) != 0) {
		printf("File was not opened\n");
		exit(EXIT_FAILURE);
	}
 
 	char line [128];
 	int total_fuel = 0;

 	while ( fgets ( line, sizeof line, fp ) != NULL )       {
 		total_fuel += calculate_fuel(atoi(line));
     }
	
	printf("%d", total_fuel);

	fclose(fp);
	
	exit(EXIT_SUCCESS);
}
