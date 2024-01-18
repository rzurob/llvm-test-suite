
#include <stdio.h>
#include <stdlib.h>

   #include "cmplx.h"

#include <inttypes.h>
#include <stddef.h>

  #define int_fast16_t short

typedef struct dt1 DT1;
typedef struct dt2 DT2;

#define DIM1 2
#define DIM2 3

struct dt1 {
   int_fast16_t var_a[DIM1][DIM2];
   double _Complex var_b[DIM1][DIM2];
   signed char var_c[DIM1][DIM2];
   float _Complex var_d[DIM1][DIM2];
   long double _Complex var_e[DIM1][DIM2];
   double _Complex var_f[DIM1][DIM2];
   intmax_t var_g[DIM1][DIM2];
   float _Complex var_h[DIM1][DIM2];
};

struct dt2 {
   float _Complex var_a[DIM1][DIM2];
   short var_b[DIM1][DIM2];
   long double _Complex var_c[DIM1][DIM2];
   double _Complex var_d[DIM1][DIM2];
   float _Complex var_e[DIM1][DIM2];
   char var_f[DIM1][DIM2];
   double _Complex var_g[DIM1][DIM2];
   int var_h[DIM1][DIM2];
   DT1 vdt1[DIM2][DIM1];
};

void sub1(DT2 *dt) {
   int i, j, k;

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       if ( (dt+k)->var_a[i][j] != createcomplexf((float)(i*DIM2+j+1),(float)(i*DIM2+j+1)) ) exit(21);
       if ( (dt+k)->var_b[i][j] != i*DIM2+j+2 ) exit(23);
       if ( (dt+k)->var_c[i][j] != createcomplexl((long double)(i*DIM2+j+3),(long double)(i*DIM2+j+3)) ) exit(25);
       if ( (dt+k)->var_d[i][j] != createcomplex((double)(i*DIM2+j+4),(double)(i*DIM2+j+4)) ) exit(27);
       if ( (dt+k)->var_e[i][j] != createcomplexf((float)(i*DIM2+j+5),(float)(i*DIM2+j+5)) ) exit(29);
       if ( (dt+k)->var_f[i][j] != i*DIM2+j+6 ) exit(31);
       if ( (dt+k)->var_g[i][j] != createcomplex((double)(i*DIM2+j+7),(double)(i*DIM2+j+7)) ) exit(33);
       if ( (dt+k)->var_h[i][j] != i*DIM2+j+8 ) exit(35);

       if ( (dt+k)->vdt1[j][i].var_a[i][j] != i*DIM2+j+1 ) exit(37);
       if ( (dt+k)->vdt1[j][i].var_b[i][j] != createcomplex((double)(i*DIM2+j+2),(double)(i*DIM2+j+2)) ) exit(39);
       if ( (dt+k)->vdt1[j][i].var_c[i][j] != i*DIM2+j+3 ) exit(41);
       if ( (dt+k)->vdt1[j][i].var_d[i][j] != createcomplexf((float)(i*DIM2+j+4),(float)(i*DIM2+j+4)) ) exit(43);
       if ( (dt+k)->vdt1[j][i].var_e[i][j] != createcomplexl((long double)(i*DIM2+j+5),(long double)(i*DIM2+j+5)) ) exit(45);
       if ( (dt+k)->vdt1[j][i].var_f[i][j] != createcomplex((double)(i*DIM2+j+6),(double)(i*DIM2+j+6)) ) exit(47);
       if ( (dt+k)->vdt1[j][i].var_g[i][j] != i*DIM2+j+7 ) exit(49);
       if ( (dt+k)->vdt1[j][i].var_h[i][j] != createcomplexf((float)(i*DIM2+j+8),(float)(i*DIM2+j+8)) ) exit(51);
     }
    }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       (dt+k)->var_a[i][j] += createcomplexf(1.0f,1.0f);
       (dt+k)->var_b[i][j] += 1;
       (dt+k)->var_c[i][j] += createcomplexl(1.0l,1.0l);
       (dt+k)->var_d[i][j] += createcomplex(1.0,1.0);
       (dt+k)->var_e[i][j] += createcomplexf(1.0f,1.0f);
       (dt+k)->var_f[i][j] += 1;
       (dt+k)->var_g[i][j] += createcomplex(1.0,1.0);
       (dt+k)->var_h[i][j] += 1;

       (dt+k)->vdt1[j][i].var_a[i][j] += 2;
       (dt+k)->vdt1[j][i].var_b[i][j] += createcomplex(2.0,2.0);
       (dt+k)->vdt1[j][i].var_c[i][j] += 2;
       (dt+k)->vdt1[j][i].var_d[i][j] += createcomplexf(2.0f,2.0f);
       (dt+k)->vdt1[j][i].var_e[i][j] += createcomplexl(2.0l,2.0l);
       (dt+k)->vdt1[j][i].var_f[i][j] += createcomplex(2.0,2.0);
       (dt+k)->vdt1[j][i].var_g[i][j] += 2;
       (dt+k)->vdt1[j][i].var_h[i][j] += createcomplexf(2.0f,2.0f);
     }
    }
   }

}

void sub2(DT2 *dtx, DT2 *dty) {
   int i, j, k;

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       if ( (dtx+k)->var_a[i][j] != createcomplexf((float)(2*(i*DIM2+j+1)),(float)(2*(i*DIM2+j+1))) ) exit(53);
       if ( (dtx+k)->var_b[i][j] != 2*(i*DIM2+j+2) ) exit(55);
       if ( (dtx+k)->var_c[i][j] != createcomplexl((long double)(2*(i*DIM2+j+3)),(long double)(2*(i*DIM2+j+3))) ) exit(57);
       if ( (dtx+k)->var_d[i][j] != createcomplex((double)(2*(i*DIM2+j+4)),(double)(2*(i*DIM2+j+4))) ) exit(59);
       if ( (dtx+k)->var_e[i][j] != createcomplexf((float)(2*(i*DIM2+j+5)),(float)(2*(i*DIM2+j+5))) ) exit(61);
       if ( (dtx+k)->var_f[i][j] != 2*(i*DIM2+j+6) ) exit(63);
       if ( (dtx+k)->var_g[i][j] != createcomplex((double)(2*(i*DIM2+j+7)),(double)(2*(i*DIM2+j+7))) ) exit(65);
       if ( (dtx+k)->var_h[i][j] != 2*(i*DIM2+j+8) ) exit(67);

       if ( (dtx+k)->vdt1[j][i].var_a[i][j] != 2*(i*DIM2+j+1) ) exit(69);
       if ( (dtx+k)->vdt1[j][i].var_b[i][j] != createcomplex((double)(2*(i*DIM2+j+2)),(double)(2*(i*DIM2+j+2))) ) exit(71);
       if ( (dtx+k)->vdt1[j][i].var_c[i][j] != 2*(i*DIM2+j+3) ) exit(73);
       if ( (dtx+k)->vdt1[j][i].var_d[i][j] != createcomplexf((float)(2*(i*DIM2+j+4)),(float)(2*(i*DIM2+j+4))) ) exit(75);
       if ( (dtx+k)->vdt1[j][i].var_e[i][j] != createcomplexl((long double)(2*(i*DIM2+j+5)),(long double)(2*(i*DIM2+j+5))) ) exit(77);
       if ( (dtx+k)->vdt1[j][i].var_f[i][j] != createcomplex((double)(2*(i*DIM2+j+6)),(double)(2*(i*DIM2+j+6))) ) exit(79);
       if ( (dtx+k)->vdt1[j][i].var_g[i][j] != 2*(i*DIM2+j+7) ) exit(81);
       if ( (dtx+k)->vdt1[j][i].var_h[i][j] != createcomplexf((float)(2*(i*DIM2+j+8)),(float)(2*(i*DIM2+j+8))) ) exit(83);
     }
    }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       (dty+k)->var_a[i][j] = (dtx+k)->var_a[i][j] + createcomplexf(1.0f,1.0f);
       (dty+k)->var_b[i][j] = (dtx+k)->var_b[i][j] + 1;
       (dty+k)->var_c[i][j] = (dtx+k)->var_c[i][j] + createcomplexl(1.0l,1.0l);
       (dty+k)->var_d[i][j] = (dtx+k)->var_d[i][j] + createcomplex(1.0,1.0);
       (dty+k)->var_e[i][j] = (dtx+k)->var_e[i][j] + createcomplexf(1.0f,1.0f);
       (dty+k)->var_f[i][j] = (dtx+k)->var_f[i][j] + 1;
       (dty+k)->var_g[i][j] = (dtx+k)->var_g[i][j] + createcomplex(1.0,1.0);
       (dty+k)->var_h[i][j] = (dtx+k)->var_h[i][j] + 1;

       (dty+k)->vdt1[j][i].var_a[i][j] = (dtx+k)->vdt1[j][i].var_a[i][j] + 2;
       (dty+k)->vdt1[j][i].var_b[i][j] = (dtx+k)->vdt1[j][i].var_b[i][j] + createcomplex(2.0,2.0);
       (dty+k)->vdt1[j][i].var_c[i][j] = (dtx+k)->vdt1[j][i].var_c[i][j] + 2;
       (dty+k)->vdt1[j][i].var_d[i][j] = (dtx+k)->vdt1[j][i].var_d[i][j] + createcomplexf(2.0f,2.0f);
       (dty+k)->vdt1[j][i].var_e[i][j] = (dtx+k)->vdt1[j][i].var_e[i][j] + createcomplexl(2.0l,2.0l);
       (dty+k)->vdt1[j][i].var_f[i][j] = (dtx+k)->vdt1[j][i].var_f[i][j] + createcomplex(2.0,2.0);
       (dty+k)->vdt1[j][i].var_g[i][j] = (dtx+k)->vdt1[j][i].var_g[i][j] + 2;
       (dty+k)->vdt1[j][i].var_h[i][j] = (dtx+k)->vdt1[j][i].var_h[i][j] + createcomplexf(2.0f,2.0f);
     }
    }
   }

}

