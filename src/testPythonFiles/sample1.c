#include ".//sample1.h"

int main(){
			printf("%s", "Welcome to sample1.py");
	int userInput = 0;
//should address this as a limitation
	userInput = getUserInput();
	printf("%s", "Incrementing to 100...");
	int increment = 0;
//should address this as a limitation
	increment = getIncrementTo100(userInput);
	return 0;
}
