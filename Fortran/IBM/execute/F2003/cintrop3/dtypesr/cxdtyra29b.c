
#include <stdio.h>
#include <stdlib.h>

#ifdef STDINT
   #include <stdint.h>
   #include <stddef.h>
#endif

#if ( defined(_AIX) && ! defined(_AIX52) )
  #define int_fast16_t short
#endif

typedef struct dt1 DT1;
typedef struct dt2 DT2;

#define DIM1 2
#define DIM2 3

struct dt1 {
   int_fast16_t var_a[DIM1][DIM2];
   double var_b[DIM1][DIM2];
   signed char var_c[DIM1][DIM2];
   float var_d[DIM1][DIM2];
   long double var_e[DIM1][DIM2];
   double var_f[DIM1][DIM2];
   intmax_t var_g[DIM1][DIM2];
   float var_h[DIM1][DIM2];
};

struct dt2 {
   float var_a[DIM1][DIM2];
   short var_b[DIM1][DIM2];
   long double var_c[DIM1][DIM2];
   double var_d[DIM1][DIM2];
   float var_e[DIM1][DIM2];
   char var_f[DIM1][DIM2];
   double var_g[DIM1][DIM2];
   int var_h[DIM1][DIM2];
   DT1 vdt1[DIM2][DIM1];
};

DT2 *fun1(DT2 *dt) {
   int i, j, k;

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       if ( (dt+k)->var_a[i][j] != (float)(i*DIM2+j+1) ) exit(21);
       if ( (dt+k)->var_b[i][j] != i*DIM2+j+2 ) exit(23);
       if ( (dt+k)->var_c[i][j] != (long double)(i*DIM2+j+3) ) exit(25);
       if ( (dt+k)->var_d[i][j] != (double)(i*DIM2+j+4) ) exit(27);
       if ( (dt+k)->var_e[i][j] != (float)(i*DIM2+j+5) ) exit(29);
       if ( (dt+k)->var_f[i][j] != i*DIM2+j+6 ) exit(31);
       if ( (dt+k)->var_g[i][j] != (double)(i*DIM2+j+7) ) exit(33);
       if ( (dt+k)->var_h[i][j] != i*DIM2+j+8 ) exit(35);

       if ( (dt+k)->vdt1[j][i].var_a[i][j] != i*DIM2+j+1 ) exit(37);
       if ( (dt+k)->vdt1[j][i].var_b[i][j] != (double)(i*DIM2+j+2) ) exit(39);
       if ( (dt+k)->vdt1[j][i].var_c[i][j] != i*DIM2+j+3 ) exit(41);
       if ( (dt+k)->vdt1[j][i].var_d[i][j] != (float)(i*DIM2+j+4) ) exit(43);
       if ( (dt+k)->vdt1[j][i].var_e[i][j] != (long double)(i*DIM2+j+5) ) exit(45);
       if ( (dt+k)->vdt1[j][i].var_f[i][j] != (double)(i*DIM2+j+6) ) exit(47);
       if ( (dt+k)->vdt1[j][i].var_g[i][j] != i*DIM2+j+7 ) exit(49);
       if ( (dt+k)->vdt1[j][i].var_h[i][j] != (float)(i*DIM2+j+8) ) exit(51);
     }
    }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       (dt+k)->var_a[i][j] += 1.0f;
       (dt+k)->var_b[i][j] += 1;
       (dt+k)->var_c[i][j] += 1.0l;
       (dt+k)->var_d[i][j] += 1.0;
       (dt+k)->var_e[i][j] += 1.0f;
       (dt+k)->var_f[i][j] += 1;
       (dt+k)->var_g[i][j] += 1.0;
       (dt+k)->var_h[i][j] += 1;

       (dt+k)->vdt1[j][i].var_a[i][j] += 2;
       (dt+k)->vdt1[j][i].var_b[i][j] += 2.0;
       (dt+k)->vdt1[j][i].var_c[i][j] += 2;
       (dt+k)->vdt1[j][i].var_d[i][j] += 2.0f;
       (dt+k)->vdt1[j][i].var_e[i][j] += 2.0l;
       (dt+k)->vdt1[j][i].var_f[i][j] += 2.0;
       (dt+k)->vdt1[j][i].var_g[i][j] += 2;
       (dt+k)->vdt1[j][i].var_h[i][j] += 2.0f;
     }
    }
   }
   return(dt);
}