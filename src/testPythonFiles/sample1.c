#include ".//testPythonFiles/sample1.h"
int sampleFunction(int a, int b){
	return a;
}
void callFunction(){
	return sampleFunction(10, 5);
}
int main(){
	callFunction();
}
