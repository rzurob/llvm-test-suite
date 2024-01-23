
#include <stdio.h>
#include <stdlib.h>

typedef struct dt1 DT1;

#define DIM1 2
#define DIM2 3

struct dt1 {
   float var_a[DIM1][DIM2];
   _Bool var_b[DIM1][DIM2];
   long double var_c[DIM1][DIM2];
   double var_d[DIM1][DIM2];
   float var_e[DIM1][DIM2];
   _Bool var_f[DIM1][DIM2];
   double var_g[DIM1][DIM2];
   char var_h[DIM1][DIM2];
};

void sub1(DT1 *dt) {
   int i, j, k;

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       if ( (dt+k)->var_a[i][j] != (float)(i*DIM2+j+1) ) exit(21);
       if ( (dt+k)->var_b[i][j] != 1 ) exit(23);
       if ( (dt+k)->var_c[i][j] != (long double)(i*DIM2+j+3) ) exit(25);
       if ( (dt+k)->var_d[i][j] != (double)(i*DIM2+j+4) ) exit(27);
       if ( (dt+k)->var_e[i][j] != (float)(i*DIM2+j+5) ) exit(29);
       if ( (dt+k)->var_f[i][j] != 1 ) exit(31);
       if ( (dt+k)->var_g[i][j] != (double)(i*DIM2+j+7) ) exit(33);
       if ( (dt+k)->var_h[i][j] != 'A' ) exit(35);
     }
    }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       (dt+k)->var_a[i][j] += 1.0f;
       (dt+k)->var_b[i][j] = 0;
       (dt+k)->var_c[i][j] += 1.0l;
       (dt+k)->var_d[i][j] += 1.0;
       (dt+k)->var_e[i][j] += 1.0f;
       (dt+k)->var_f[i][j] = 0;
       (dt+k)->var_g[i][j] += 1.0;
       (dt+k)->var_h[i][j] = 'B';
     }
    }
   }

}

void sub2(DT1 *dtx, DT1 *dty) {
   int i, j, k;

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       if ( (dtx+k)->var_a[i][j] != (float)(2*(i*DIM2+j+1)) ) exit(37);
       if ( (dtx+k)->var_b[i][j] != 1 ) exit(39);
       if ( (dtx+k)->var_c[i][j] != (long double)(2*(i*DIM2+j+3)) ) exit(41);
       if ( (dtx+k)->var_d[i][j] != (double)(2*(i*DIM2+j+4)) ) exit(43);
       if ( (dtx+k)->var_e[i][j] != (float)(2*(i*DIM2+j+5)) ) exit(45);
       if ( (dtx+k)->var_f[i][j] != 1 ) exit(47);
       if ( (dtx+k)->var_g[i][j] != (double)(2*(i*DIM2+j+7)) ) exit(49);
       if ( (dtx+k)->var_h[i][j] != 'A' ) exit(51);
     }
    }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
       (dty+k)->var_a[i][j] = (dtx+k)->var_a[i][j] + 1.0f;
       (dty+k)->var_b[i][j] = 0;
       (dty+k)->var_c[i][j] = (dtx+k)->var_c[i][j] + 1.0l;
       (dty+k)->var_d[i][j] = (dtx+k)->var_d[i][j] + 1.0;
       (dty+k)->var_e[i][j] = (dtx+k)->var_e[i][j] + 1.0f;
       (dty+k)->var_f[i][j] = 0;
       (dty+k)->var_g[i][j] = (dtx+k)->var_g[i][j] + 1.0;
       (dty+k)->var_h[i][j] = 'B';
     }
    }
   }
}
