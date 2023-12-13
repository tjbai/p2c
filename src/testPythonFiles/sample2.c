#include ".//testPythonFiles/sample2.h"
int sampleFunction(int a, int b){
	while(a > b){
		 a =- 1;
	}
	return a;
}
 callFunction(){
	return sampleFunction(10, 5);
}
int main(){
	callFunction();
}
