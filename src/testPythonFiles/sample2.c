#include ".//sample2.h"

int getIncrementTo100(int max){
	int i = 0;
	while(i < max){
		if(i % 3 == 0){
			printf("%s %d", "\ndivisible by 3", i);
		}else if(i % 2 == 0){
			printf("%s %d", "\ndivisible by 2", i);
		}else{
			printf("%s %d", "\nnot divisible by 3 or 2", i);
		}
		i += 1;
	}
	return i;
}
