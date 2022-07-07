#include<stdio.h>
#include<stdint.h>

const uint8_t foo[] = {
               #include "out.svm"
         };


int main(){
	int i;
	for (i = 0; i < sizeof(foo)/sizeof(foo[0])-1; i++){
		printf("%d,",foo[i]);
	}
	printf("%d", foo[i]);
}
