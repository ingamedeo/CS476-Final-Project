#include <stdio.h>

/*
Sample test .c source code for mem2reg LLVM pass.
*/

int main() {
    int i,b = 0;
    float a = 0;
    printf("Input b is ");
    scanf("%d", &b);
    for(i = 0; i < b; i++) {
    a=a+2;
    printf("a is %f", a);
    }

    return 0;
}
