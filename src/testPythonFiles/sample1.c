#include "sample1.h"
int sampleFunction(int a, int b) {
  if (a > b) {
    return a;
  } else {
    return b;
  }
}
int main() { sampleFunctionTwo(1, sampleFunction(1, 2)); }
