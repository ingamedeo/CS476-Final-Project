#include <stdio.h>
#define SIZE 16

int main(void){
    
    float acc = 0.0;
    #pragma clang loop unroll_count(10)
    for(int i = 0; i < 10; ++i){
        acc += i * i;
    }

    printf("Final value is %f\n", acc);
    return 0;
}
