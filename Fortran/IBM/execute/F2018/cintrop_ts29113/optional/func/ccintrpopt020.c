#include <stdlib.h>
#include "cmplx.h"

typedef struct dt1 DT1;
typedef struct dt2 DT2;

struct dt1 {
    short int           si[20];
    int                 id;
};

struct dt2 {
    int_fast8_t		if8[4]; 
    int_fast16_t	if16; 
    int_fast32_t	if32; 
    int_fast64_t	if64; 
    long double		ldouble[5];
    double _Complex     dc;
    struct dt1          d1;
};

void sub_test1(int *, unsigned char *, DT2 *);        /* only argument DT2 is optional */
void sub_test2(int *, DT2 *, DT1 *);   /* both arguments DT2 and DT1 are optionsl */

int main(int argc, char ** argv)
{   
    unsigned char ch = 'z';
    int k, len = 20;
    DT2 *dt2_a, dt2_b; 
    DT1 *dt1_a, dt1_b;
    double _Complex    pre_dc;
    
    /*data initialization */
    pre_dc =  createcomplex(5.0, 5.0);

    dt2_a = malloc(sizeof(DT2));
    
    for (k = 0; k < 4; k++) {
        dt2_a->if8[k] = k;
    }
    for (k = 0; k < 5; k++) {
        dt2_a->ldouble[k]  = k * 1.0;
    }
    dt2_a->if16 = 16;
    dt2_a->if32 = 32;
    dt2_a->if64 = 64;
    dt2_a->dc = pre_dc;
    dt2_a->d1.id = 5;
    for (k = 0; k < 20; k++) {
        dt2_a->d1.si[k] = k + dt2_a->d1.id;
    }

    for (k = 0; k < 4; k++) {
        dt2_b.if8[k] = k + k;
    }
    for (k = 0; k < 5; k++) { 
        dt2_b.ldouble[k]  = k * 5.0;
    }
    pre_dc =  createcomplex(6.0, 6.0);
    dt2_b.dc = pre_dc;
    dt2_b.if16 = 32;
    dt2_b.if32 = 64;
    dt2_b.if64 = 128;
    dt2_b.d1.id = 15;
    for (k = 0; k < 20; k++) {
        dt2_b.d1.si[k] = k + dt2_b.d1.id;
    }

    dt1_a = malloc(sizeof(DT1));

    for ( k = 0; k < 20; k++ ) {
	dt1_a->si[k] = k;
    }
    dt1_a->id = 3;
    
    for ( k = 0; k < 20; k++ ) {
       dt1_b.si[k] = k + 20;
    }
    dt1_b.id = k;

    sub_test1(&len, &ch, NULL); 
    sub_test1(&len, &ch, dt2_a);
    sub_test1(&len, &ch, &dt2_b);

    sub_test2(&len, NULL, NULL);
    sub_test2(&len, NULL, dt1_a);
    sub_test2(&len, dt2_a, &dt1_b);
   
    return 0;
}

