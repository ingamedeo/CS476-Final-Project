#include <stdio.h>

void change_value_to(int *dst, const int value){
    *dst = value;
}

int main(void){

    int a = 3;
    const int b = 42;
    change_value_to(&a, b);

    printf("a is now %d\n", a);

}