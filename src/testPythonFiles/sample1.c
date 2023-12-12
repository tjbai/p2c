#include ".//testPythonFiles/sample1.h"
int sampleFunction(int a, int b){
	for(int i=0;i<10;i=i+1){
		if(i == 5){
			printf("%s ", "i is 5");
		}else{
			printf("%s ", "i is not 5");
		}
	}
}
