#include <stdio.h>

int func_test1();
void sub_test1(int *size, float *buffer);

void c_helper()
{
    int i, size = 100;
    int actual_size;
    float *buffer;

    buffer = (float *)malloc(sizeof(float)*size);

    for (i=0; i<100; i++) {
	*(buffer+i) = i + 1.0;
    }

    sub_test1(&size, NULL);  
    actual_size = func_test1();
    if (actual_size != 0) return -200;

    actual_size = func_test1(&size, buffer);  
    if (actual_size != size) return -300;
   
    return 0;
   
}
