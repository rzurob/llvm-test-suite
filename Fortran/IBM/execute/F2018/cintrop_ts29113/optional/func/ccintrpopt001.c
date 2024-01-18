#include <stdlib.h>
#include "cmplx.h"

void sub_int(int * arg);
void sub_short(short int * arg);
void sub_long(long int* arg);
void sub_long_long(long long int* arg);
void sub_unsigned(unsigned char* arg);
void sub_size_t(size_t * arg);
void sub_int8_t(int8_t * arg);
void sub_int16_t(int16_t * arg);
void sub_int32_t(int32_t * arg);
void sub_int64_t(int64_t * arg);
void sub_int_least8_t(int_least8_t * arg);
void sub_int_least16_t(int_least16_t * arg);
void sub_int_least32_t(int_least32_t * arg);
void sub_int_least64_t(int_least64_t * arg);
void sub_int_fast8_t(int_fast8_t * arg);
void sub_int_fast16_t(int_fast16_t * arg);
void sub_int_fast32_t(int_fast32_t * arg);
void sub_int_fast64_t(int_fast64_t * arg);
void sub_intmax_t(intmax_t * arg);
void sub_intptr_t(intptr_t * arg);
void sub_float(float * arg);
void sub_double(double * arg);
void sub_long_double(long double* arg);
void sub_float_complex(float _Complex * arg);
void sub_double_complex(double _Complex * arg);
void sub_long_double_complex(long double _Complex * arg);
void sub_bool(_Bool * arg);
void sub_char(char * arg);

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
    float _Complex          c1;
    double _Complex         c2;
    long double _Complex    c3;
    _Bool           ll = 0;
    char            cc = 'c';

    c1 = createcomplexf(5.0, 5.0);
    c2 = createcomplex(6.0, 6.0);
    c3 = createcomplexl(7.0, 7.0);

    /* calling the subroutines with NULL as actual argument */
    sub_int(NULL);
    sub_short(NULL);
    sub_long(NULL);
    sub_long_long(NULL);
    sub_unsigned(NULL);
    sub_size_t(NULL);
    sub_int8_t(NULL);
    sub_int16_t(NULL);
    sub_int32_t(NULL);
    sub_int64_t(NULL);
    sub_int_least8_t(NULL);
    sub_int_least16_t(NULL);
    sub_int_least32_t(NULL);
    sub_int_least64_t(NULL);
    sub_int_fast8_t(NULL);
    sub_int_fast16_t(NULL);
    sub_int_fast32_t(NULL);
    sub_int_fast64_t(NULL);
    sub_intmax_t(NULL);
    sub_intptr_t(NULL);
    sub_float(NULL);
    sub_double(NULL);
    sub_long_double(NULL);
    sub_float_complex(NULL);
    sub_double_complex(NULL);
    sub_long_double_complex(NULL);
    sub_bool(NULL);
    sub_char(NULL);

    /* calling the subroutines with corresponding C types as actual argument */
    sub_int(&i1);
    sub_short(&i2);
    sub_long(&i3);
    sub_long_long(&i4);
    sub_unsigned(&i5);
    sub_size_t(&i6);
    sub_int8_t(&i7);
    sub_int16_t(&i8);
    sub_int32_t(&i9);
    sub_int64_t(&i10);
    sub_int_least8_t(&i11);
    sub_int_least16_t(&i12);
    sub_int_least32_t(&i13);
    sub_int_least64_t(&i14);
    sub_int_fast8_t(&i15);
    sub_int_fast16_t(&i16);
    sub_int_fast32_t(&i17);
    sub_int_fast64_t(&i18);
    sub_intmax_t(&i19);
    sub_intptr_t(&i20);
    sub_float(&r1);
    sub_double(&r2);
    sub_long_double(&r3);
    sub_float_complex(&c1);
    sub_double_complex(&c2);
    sub_long_double_complex(&c3);
    sub_bool(&ll);
    sub_char(&cc);

    return 0;
}


