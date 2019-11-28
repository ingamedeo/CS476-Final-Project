#include <stdio.h>
#include <stdlib.h>

__attribute__((always_inline))
void change_value_to(float *dst){
    *dst = rand();
}

/* __attribute__((always_inline))
inline void change_value_to2(float *dst);
 */
int main(void){

    float a = 3.0;
    change_value_to(&a);
    printf("a is now %f\n", a);

}
