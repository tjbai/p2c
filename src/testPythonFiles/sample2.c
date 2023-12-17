#include ".//sample2.h"

int getIncrementTo100(int max){
	int i = 0;
	while(i < max){
		i += 1;
		printf("%d", i);
	}
	return i;
}
int getUserInput(){
	int output = 0;
	output = scanf("%s", "Enter a number : ");
	return output;
}
