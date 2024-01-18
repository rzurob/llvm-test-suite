#include <stdlib.h>
#include <complex.h>
#include <stdint.h>

void sub(void * a);

int main(int argc, char ** argv)
{   
    int             i1 = 1;
    short int       i2 = 2;
    long int        i3 = 3;
    long long int   i4 = 4;
    unsigned char   i5 = 5;
    size_t          i6 = 6;
    int8_t          i7 = 7;
    int16_t         i8 = 8;
    int32_t         i9 = 9;
    int64_t         i10 = 10;
    int_least8_t    i11 = 11;
    int_least16_t   i12 = 12;
    int_least32_t   i13 = 13;
    int_least64_t   i14 = 14;
    int_fast8_t     i15 = 15;
    int_fast16_t    i16 = 16;
    int_fast32_t    i17 = 17;
    int_fast64_t    i18 = 18;
    intmax_t        i19 = 19;
    intptr_t        i20 = 20;
    float           r1 = 1.1;
    double          r2 = 1.2;
    long double     r3 = 1.3;
    _Bool           ll = 0;
    char            cc = 'c';

    sub(&i1);
    sub(&i2);
    sub(&i3);
    sub(&i4);
    sub(&i5);
    sub(&i6);
    sub(&i7);
    sub(&i8);
    sub(&i9);
    sub(&i10);
    sub(&i11);
    sub(&i12);
    sub(&i13);
    sub(&i14);
    sub(&i15);
    sub(&i16);
    sub(&i17);
    sub(&i18);
    sub(&i19);
    sub(&i20);
    sub(&r1);
    sub(&r2);
    sub(&r3);
    sub(&ll);
    sub(&cc);

    return 0;
}
