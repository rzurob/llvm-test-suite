#include <stdlib.h>
#include "cmplx.h"

void cfun(void (*fproc[])())
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
    for(short i=0; (*fproc[i]) != NULL; i++) (*fproc[i])(NULL);

//    (*fproc[0])(NULL);    // sub_int(NULL);
//    (*fproc[1])(NULL);    // sub_short(NULL);
//    (*fproc[2])(NULL);    // sub_long(NULL);
//    (*fproc[3])(NULL);    // sub_long_long(NULL);
//    (*fproc[4])(NULL);    // sub_unsigned(NULL);
//    (*fproc[5])(NULL);    // sub_size_t(NULL);
//    (*fproc[6])(NULL);    // sub_int8_t(NULL);
//    (*fproc[7])(NULL);    // sub_int16_t(NULL);
//    (*fproc[8])(NULL);    // sub_int32_t(NULL);
//    (*fproc[9])(NULL);    // sub_int64_t(NULL);
//    (*fproc[10])(NULL);   // sub_int_least8_t(NULL);
//    (*fproc[11])(NULL);   // sub_int_least16_t(NULL);
//    (*fproc[12])(NULL);   // sub_int_least32_t(NULL);
//    (*fproc[13])(NULL);   // sub_int_least64_t(NULL);
//    (*fproc[14])(NULL);   // sub_int_fast8_t(NULL);
//    (*fproc[15])(NULL);   // sub_int_fast16_t(NULL);
//    (*fproc[16])(NULL);   // sub_int_fast32_t(NULL);
//    (*fproc[17])(NULL);   // sub_int_fast64_t(NULL);
//    (*fproc[18])(NULL);   // sub_intmax_t(NULL);
//    (*fproc[19])(NULL);   // sub_intptr_t(NULL);
//    (*fproc[20])(NULL);   // sub_float(NULL);
//    (*fproc[21])(NULL);   // sub_double(NULL);
//    (*fproc[22])(NULL);   // sub_long_double(NULL);
//    (*fproc[23])(NULL);   // sub_float_complex(NULL);
//    (*fproc[24])(NULL);   // sub_double_complex(NULL);
//    (*fproc[25])(NULL);   // sub_long_double_complex(NULL);
//    (*fproc[26])(NULL);   // sub_bool(NULL);
//    (*fproc[27])(NULL);   // sub_char(NULL);

    /* calling the subroutines with corresponding C types as actual argument */
     (*fproc[0])(&i1);    // sub_int(%i1);
     (*fproc[1])(&i2);    // sub_short(&i2);
     (*fproc[2])(&i3);    // sub_long(&i3);
     (*fproc[3])(&i4);    // sub_long_long(&i4);
     (*fproc[4])(&i5);    // sub_unsigned(&i5);
     (*fproc[5])(&i6);    // sub_size_t(&i6);
     (*fproc[6])(&i7);    // sub_int8_t(&i7);
     (*fproc[7])(&i8);    // sub_int16_t(&i8);
     (*fproc[8])(&i9);    // sub_int32_t(&i9);
     (*fproc[9])(&i10);   // sub_int64_t(&i10);
     (*fproc[10])(&i11);  // sub_int_least8_t(&i11);
     (*fproc[11])(&i12);  // sub_int_least16_t(&i12);
     (*fproc[12])(&i13);  // sub_int_least32_t(&i13);
     (*fproc[13])(&i14);  // sub_int_least64_t(&i14);
     (*fproc[14])(&i15);  // sub_int_fast8_t(&i15);
     (*fproc[15])(&i16);  // sub_int_fast16_t(&i16);
     (*fproc[16])(&i17);  // sub_int_fast32_t(&i17);
     (*fproc[17])(&i18);  // sub_int_fast64_t(&i18);
     (*fproc[18])(&i19);  // sub_intmax_t(&i19);
     (*fproc[19])(&i20);  // sub_intptr_t(&i20);
     (*fproc[20])(&r1);   // sub_float(&r1);
     (*fproc[21])(&r2);   // sub_double(&r2);
     (*fproc[22])(&r3);   // sub_long_double(&r3);
     (*fproc[23])(&c1);   // sub_float_complex(&c1);
     (*fproc[24])(&c2);   // sub_double_complex(&c2);
     (*fproc[25])(&c3);   // sub_long_double_complex(&c3);
     (*fproc[26])(&ll);   // sub_bool(&ll);
     (*fproc[27])(&cc);   // sub_char(&cc);
}
