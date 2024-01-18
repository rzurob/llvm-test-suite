#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <complex.h>


size_t get_size_r4(float a  ) {
    return sizeof(a);
}

size_t get_size_r8(double a  ) {
    return sizeof(a);
}

size_t get_size_i2(short a) {
    return sizeof(a);
}

size_t get_size_i8(long long int a[4]) {
    return 4*sizeof(long long int);
}

size_t get_size_c4(float _Complex a[2]  ) {
    return sizeof(a[0])*2;
}

