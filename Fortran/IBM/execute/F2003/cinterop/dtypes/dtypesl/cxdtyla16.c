
#include <stdio.h>
#include <stdlib.h>

#define DIM1 2
#define DIM2 3

typedef struct dt1 DT1;

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

#define ARRDT1 {1.0f,2.0f,3.0f,4.0f,5.0f,6.0f}, {0,0,0,0,0,0}, {3.0l,4.0l,5.0l,6.0l,7.0l,8.0l}, {4.0,5.0,6.0,7.0,8.0,9.0}, {5.0f,6.0f,7.0f,8.0f,9.0f,10.0f}, {0,0,0,0,0,0}, {7.0,8.0,9.0,10.0,11.0,12.0}, {'A','A','A','A','A','A'}

DT1 *stsum(DT1 *, DT1 *);

int main() {

   void sub1(DT1 *);
   void sub2(DT1 *, DT1 *);

   DT1 dt0 = { ARRDT1 };

   DT1 dta[DIM2][DIM1], dtb[DIM2][DIM1];
   DT1 *dtp;
   int i, j, k, l;


   for ( i = 0; i < DIM2; i++ ) {
     for ( j = 0; j < DIM1; j++ ) {
       dta[i][j] = dt0;
     }
   }

   sub1(&dta[0][0]);

   for ( k = 0; k < DIM2; k++ ) {
    for ( l = 0; l < DIM1; l++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
       if ( dta[k][l].var_a[i][j] != (float)(i*DIM2+j+2) ) exit(21);
       if ( dta[k][l].var_b[i][j] != 1 ) exit(23);
       if ( dta[k][l].var_c[i][j] != (long double)(i*DIM2+j+4) ) exit(25);
       if ( dta[k][l].var_d[i][j] != (double)(i*DIM2+j+5) ) exit(27);
       if ( dta[k][l].var_e[i][j] != (float)(i*DIM2+j+6) ) exit(29);
       if ( dta[k][l].var_f[i][j] != 1 ) exit(31);
       if ( dta[k][l].var_g[i][j] != (double)(i*DIM2+j+8) ) exit(33);
       if ( dta[k][l].var_h[i][j] != 'B' ) exit(35);
      }
     }
    }
   }


   for ( i = 0; i < DIM2; i++ ) {
     for ( j = 0; j < DIM1; j++ ) {
       dta[i][j] = dt0;
     }
   }

   dtp = malloc(sizeof(DT1)*DIM1*DIM2);
   dtp = stsum(&dta[0][0],&dta[0][0]);

   sub2(dtp,&dtb[0][0]);

   for ( k = 0; k < DIM2; k++ ) {
    for ( l = 0; l < DIM1; l++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
       if ( dtb[k][l].var_a[i][j] != (float)(2*(i*DIM2+j+1)+1) ) exit(37);
       if ( dtb[k][l].var_b[i][j] != 1 ) exit(39);
       if ( dtb[k][l].var_c[i][j] != (long double)(2*(i*DIM2+j+3)+1) ) exit(41);
       if ( dtb[k][l].var_d[i][j] != (double)(2*(i*DIM2+j+4)+1) ) exit(43);
       if ( dtb[k][l].var_e[i][j] != (float)(2*(i*DIM2+j+5)+1) ) exit(45);
       if ( dtb[k][l].var_f[i][j] != 1 ) exit(47);
       if ( dtb[k][l].var_g[i][j] != (double)(2*(i*DIM2+j+7)+1) ) exit(49);
       if ( dtb[k][l].var_h[i][j] != 'B' ) exit(51);
      }
     }
    }
   }

   return 0;
}

DT1 *stsum(DT1 *dtx, DT1 *dty) {
   DT1 *dtp;
   int i, j, k;

   dtp = malloc(sizeof(DT1)*DIM1*DIM2);

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
      (dtp+k)->var_a[i][j] = (dtx+k)->var_a[i][j] + (dty+k)->var_a[i][j];
      (dtp+k)->var_b[i][j] = (dtx+k)->var_b[i][j] && (dty+k)->var_b[i][j];
      (dtp+k)->var_c[i][j] = (dtx+k)->var_c[i][j] + (dty+k)->var_c[i][j];
      (dtp+k)->var_d[i][j] = (dtx+k)->var_d[i][j] + (dty+k)->var_d[i][j];
      (dtp+k)->var_e[i][j] = (dtx+k)->var_e[i][j] + (dty+k)->var_e[i][j];
      (dtp+k)->var_f[i][j] = (dtx+k)->var_f[i][j] && (dty+k)->var_f[i][j];
      (dtp+k)->var_g[i][j] = (dtx+k)->var_g[i][j] + (dty+k)->var_g[i][j];
      (dtp+k)->var_h[i][j] = (dtx+k)->var_h[i][j];
     }
    }
   }

   return(dtp);
}
