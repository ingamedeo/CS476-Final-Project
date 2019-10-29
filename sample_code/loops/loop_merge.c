#include <stdio.h>
#define SIZE 16

int main(void){
    
    float acc = 0.0;
    
    #pragma unroll
    for(int i = 0; i < SIZE; ++i){
        acc += i;
    }

    #pragma unroll
    for(int i = 0; i < SIZE; ++i){
        acc += i * i;
    }

    printf("Final value is %f\n", acc);
    return 0;
}