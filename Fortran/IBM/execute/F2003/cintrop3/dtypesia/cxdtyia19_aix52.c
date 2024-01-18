
#include <stdio.h>
#include <stdlib.h>

#define DIM1 2
#define DIM2 3

typedef struct dt1 DT1;

struct dt1 {
   char var_a[DIM1][DIM2];
   int var_b[DIM1][DIM2];
   short var_c[DIM1][DIM2];
   long long var_d[DIM1][DIM2];
   long var_e[DIM1][DIM2];
   short var_f[DIM1][DIM2];
   long long var_g[DIM1][DIM2];
   int var_h[DIM1][DIM2];
};

#define ARRDT1 {1,2,3,4,5,6}, {2,3,4,5,6,7}, {3,4,5,6,7,8}, {4,5,6,7,8,9}, {5,6,7,8,9,10}, {6,7,8,9,10,11}, {7,8,9,10,11,12}, {8,9,10,11,12,13}

DT1 *stsum(DT1 *, DT1 *);

int main() {

   DT1 *fun1(DT1 *);
   DT1 *fun2(DT1 *, DT1 *);

   DT1 dt0 = { ARRDT1 };

   DT1 *dta, *dtb, *dtp;
   int i, j, k, l;

/* Test 1 */

   dta = malloc(sizeof(DT1)*DIM2*DIM1);

   for ( k = 0; k < DIM2*DIM1; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
      (dta+k)->var_a[i][j] = dt0.var_a[i][j];
      (dta+k)->var_b[i][j] = dt0.var_b[i][j];
      (dta+k)->var_c[i][j] = dt0.var_c[i][j];
      (dta+k)->var_d[i][j] = dt0.var_d[i][j];
      (dta+k)->var_e[i][j] = dt0.var_e[i][j];
      (dta+k)->var_f[i][j] = dt0.var_f[i][j];
      (dta+k)->var_g[i][j] = dt0.var_g[i][j];
      (dta+k)->var_h[i][j] = dt0.var_h[i][j];
     }
    }
   }

   dtp = malloc(sizeof(DT1)*DIM1*DIM2);
   dtp = fun1(dta);

   for ( k = 0; k < DIM2*DIM1; k++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
       if ( (dta+k)->var_a[i][j] != i*DIM2+j+2 ) exit(21);
       if ( (dta+k)->var_b[i][j] != i*DIM2+j+3 ) exit(23);
       if ( (dta+k)->var_c[i][j] != i*DIM2+j+4 ) exit(25);
       if ( (dta+k)->var_d[i][j] != i*DIM2+j+5 ) exit(27);
       if ( (dta+k)->var_e[i][j] != i*DIM2+j+6 ) exit(29);
       if ( (dta+k)->var_f[i][j] != i*DIM2+j+7 ) exit(31);
       if ( (dta+k)->var_g[i][j] != i*DIM2+j+8 ) exit(33);
       if ( (dta+k)->var_h[i][j] != i*DIM2+j+9 ) exit(35);
      }
     }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
       if ( (dtp+k)->var_a[i][j] != i*DIM2+j+2 ) exit(37);
       if ( (dtp+k)->var_b[i][j] != i*DIM2+j+3 ) exit(39);
       if ( (dtp+k)->var_c[i][j] != i*DIM2+j+4 ) exit(41);
       if ( (dtp+k)->var_d[i][j] != i*DIM2+j+5 ) exit(43);
       if ( (dtp+k)->var_e[i][j] != i*DIM2+j+6 ) exit(45);
       if ( (dtp+k)->var_f[i][j] != i*DIM2+j+7 ) exit(47);
       if ( (dtp+k)->var_g[i][j] != i*DIM2+j+8 ) exit(49);
       if ( (dtp+k)->var_h[i][j] != i*DIM2+j+9 ) exit(51);
      }
     }
   }

/* Test 2 */

   dta = malloc(sizeof(DT1)*DIM2*DIM1);

   for ( k = 0; k < DIM2*DIM1; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
      (dta+k)->var_a[i][j] = dt0.var_a[i][j];
      (dta+k)->var_b[i][j] = dt0.var_b[i][j];
      (dta+k)->var_c[i][j] = dt0.var_c[i][j];
      (dta+k)->var_d[i][j] = dt0.var_d[i][j];
      (dta+k)->var_e[i][j] = dt0.var_e[i][j];
      (dta+k)->var_f[i][j] = dt0.var_f[i][j];
      (dta+k)->var_g[i][j] = dt0.var_g[i][j];
      (dta+k)->var_h[i][j] = dt0.var_h[i][j];
     }
    }
   }

   dtp = malloc(sizeof(DT1)*DIM1*DIM2);
   dtb = malloc(sizeof(DT1)*DIM1*DIM2);
   dtp = stsum(dta,dta);

   dtp = fun2(dtp,dtb);

   for ( k = 0; k < DIM2*DIM1; k++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
       if ( (dtb+k)->var_a[i][j] != 2*(i*DIM2+j+1)+1 ) exit(53);
       if ( (dtb+k)->var_b[i][j] != 2*(i*DIM2+j+2)+1 ) exit(55);
       if ( (dtb+k)->var_c[i][j] != 2*(i*DIM2+j+3)+1 ) exit(57);
       if ( (dtb+k)->var_d[i][j] != 2*(i*DIM2+j+4)+1 ) exit(59);
       if ( (dtb+k)->var_e[i][j] != 2*(i*DIM2+j+5)+1 ) exit(61);
       if ( (dtb+k)->var_f[i][j] != 2*(i*DIM2+j+6)+1 ) exit(63);
       if ( (dtb+k)->var_g[i][j] != 2*(i*DIM2+j+7)+1 ) exit(65);
       if ( (dtb+k)->var_h[i][j] != 2*(i*DIM2+j+8)+1 ) exit(67);
      }
     }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
       if ( (dtp+k)->var_a[i][j] != 2*(i*DIM2+j+1)+1 ) exit(69);
       if ( (dtp+k)->var_b[i][j] != 2*(i*DIM2+j+2)+1 ) exit(71);
       if ( (dtp+k)->var_c[i][j] != 2*(i*DIM2+j+3)+1 ) exit(73);
       if ( (dtp+k)->var_d[i][j] != 2*(i*DIM2+j+4)+1 ) exit(75);
       if ( (dtp+k)->var_e[i][j] != 2*(i*DIM2+j+5)+1 ) exit(77);
       if ( (dtp+k)->var_f[i][j] != 2*(i*DIM2+j+6)+1 ) exit(79);
       if ( (dtp+k)->var_g[i][j] != 2*(i*DIM2+j+7)+1 ) exit(81);
       if ( (dtp+k)->var_h[i][j] != 2*(i*DIM2+j+8)+1 ) exit(83);
      }
     }
   }

   free(dta);
   free(dtp);

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
      (dtp+k)->var_b[i][j] = (dtx+k)->var_b[i][j] + (dty+k)->var_b[i][j];
      (dtp+k)->var_c[i][j] = (dtx+k)->var_c[i][j] + (dty+k)->var_c[i][j];
      (dtp+k)->var_d[i][j] = (dtx+k)->var_d[i][j] + (dty+k)->var_d[i][j];
      (dtp+k)->var_e[i][j] = (dtx+k)->var_e[i][j] + (dty+k)->var_e[i][j];
      (dtp+k)->var_f[i][j] = (dtx+k)->var_f[i][j] + (dty+k)->var_f[i][j];
      (dtp+k)->var_g[i][j] = (dtx+k)->var_g[i][j] + (dty+k)->var_g[i][j];
      (dtp+k)->var_h[i][j] = (dtx+k)->var_h[i][j] + (dty+k)->var_h[i][j];
     }
    }
   }

   return(dtp);
}
