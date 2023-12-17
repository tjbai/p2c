#include ".//sample1.h"

int main() {
  printf("%s", "Welcome to sample1.py\n");
  int userInput = 0;
  userInput = getUserInput();
  int increment = 0;
  increment = getIncrementTo100(userInput);
  return 0;
}
