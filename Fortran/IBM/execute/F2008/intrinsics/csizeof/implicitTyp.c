#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <complex.h>


struct dType {
   short          i2[3];
   float _Complex c4[2];
   float          r4; 
};

size_t get_size_dt(struct dType dt) {
    return sizeof(dt);
}

size_t get_size_i2(short int a  ) {
    return sizeof(a);
}

size_t get_size_i4(int a  ) {
    return sizeof(a);
}

size_t get_size_i8(long long int a  ) {
    return sizeof(a);
}

size_t get_size_r4(float a  ) {
    return sizeof(a);
}

size_t get_size_r8(double a  ) {
    return sizeof(a);
}

size_t get_size_c4(float _Complex a  ) {
    return sizeof(a);
}

size_t get_size_c8(double _Complex a  ) {
    return sizeof(a);
}
