
#include <stdio.h>
#include <stdlib.h>

   #include "cmplx.h"

#define DIM1 2
#define DIM2 3

typedef struct dt1 DT1;

struct dt1 {
   float _Complex var_a[DIM1][DIM2];
   short var_b[DIM1][DIM2];
   long double _Complex var_c[DIM1][DIM2];
   double _Complex var_d[DIM1][DIM2];
   float _Complex var_e[DIM1][DIM2];
   char var_f[DIM1][DIM2];
   double _Complex var_g[DIM1][DIM2];
   int var_h[DIM1][DIM2];
};

#define ARRDT1 {createcomplexf(1.0f,1.0f),createcomplexf(2.0f,2.0f),createcomplexf(3.0f,3.0f),createcomplexf(4.0f,4.0f),createcomplexf(5.0f,5.0f),createcomplexf(6.0f,6.0f)}, {2,3,4,5,6,7}, {createcomplexl(3.0l,3.0l),createcomplexl(4.0l,4.0l),createcomplexl(5.0l,5.0l),createcomplexl(6.0l,6.0l),createcomplexl(7.0l,7.0l),createcomplexl(8.0l,8.0l)}, {createcomplex(4.0,4.0),createcomplex(5.0,5.0),createcomplex(6.0,6.0),createcomplex(7.0,7.0),createcomplex(8.0,8.0),createcomplex(9.0,9.0)}, {createcomplexf(5.0f,5.0f),createcomplexf(6.0f,6.0f),createcomplexf(7.0f,7.0f),createcomplexf(8.0f,8.0f),createcomplexf(9.0f,9.0f),createcomplexf(10.0f,10.0f)}, {6,7,8,9,10,11}, {createcomplex(7.0,7.0),createcomplex(8.0,8.0),createcomplex(9.0,9.0),createcomplex(10.0,10.0),createcomplex(11.0,11.0),createcomplex(12.0,12.0)}, {8,9,10,11,12,13}

DT1 *stsum(DT1 *, DT1 *);

int main() {

   DT1 *fun1(DT1 *);
   DT1 *fun2(DT1 *, DT1 *);

   DT1 dt0 = { ARRDT1 };

   DT1 dta[DIM2][DIM1], dtb[DIM2][DIM1], dtc[DIM2][DIM1];
   DT1 *dtp;
   int i, j, k, l;

/* Test 1 */

   for ( i = 0; i < DIM2; i++ ) {
     for ( j = 0; j < DIM1; j++ ) {
       dta[i][j] = dt0;
     }
   }

   dtp = malloc(sizeof(DT1)*DIM1*DIM2);
   dtp = fun1(&dta[0][0]);

   for ( k = 0; k < DIM2; k++ ) {
    for ( l = 0; l < DIM1; l++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
       if ( dta[k][l].var_a[i][j] != createcomplexf((float)(i*DIM2+j+2),(float)(i*DIM2+j+2)) ) exit(21);
       if ( dta[k][l].var_b[i][j] != i*DIM2+j+3 ) exit(23);
       if ( dta[k][l].var_c[i][j] != createcomplexl((long double)(i*DIM2+j+4),(long double)(i*DIM2+j+4)) ) exit(25);
       if ( dta[k][l].var_d[i][j] != createcomplex((double)(i*DIM2+j+5),(double)(i*DIM2+j+5)) ) exit(27);
       if ( dta[k][l].var_e[i][j] != createcomplexf((float)(i*DIM2+j+6),(float)(i*DIM2+j+6)) ) exit(29);
       if ( dta[k][l].var_f[i][j] != i*DIM2+j+7 ) exit(31);
       if ( dta[k][l].var_g[i][j] != createcomplex((double)(i*DIM2+j+8),(double)(i*DIM2+j+8)) ) exit(33);
       if ( dta[k][l].var_h[i][j] != i*DIM2+j+9 ) exit(35);
      }
     }
    }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
       if ( (dtp+k)->var_a[i][j] != createcomplexf((float)(i*DIM2+j+2),(float)(i*DIM2+j+2)) ) exit(37);
       if ( (dtp+k)->var_b[i][j] != i*DIM2+j+3 ) exit(39);
       if ( (dtp+k)->var_c[i][j] != createcomplexl((long double)(i*DIM2+j+4),(long double)(i*DIM2+j+4)) ) exit(41);
       if ( (dtp+k)->var_d[i][j] != createcomplex((double)(i*DIM2+j+5),(double)(i*DIM2+j+5)) ) exit(43);
       if ( (dtp+k)->var_e[i][j] != createcomplexf((float)(i*DIM2+j+6),(float)(i*DIM2+j+6)) ) exit(45);
       if ( (dtp+k)->var_f[i][j] != i*DIM2+j+7 ) exit(47);
       if ( (dtp+k)->var_g[i][j] != createcomplex((double)(i*DIM2+j+8),(double)(i*DIM2+j+8)) ) exit(49);
       if ( (dtp+k)->var_h[i][j] != i*DIM2+j+9 ) exit(51);
      }
     }
   }

/* Test 2 */

   for ( i = 0; i < DIM2; i++ ) {
     for ( j = 0; j < DIM1; j++ ) {
       dta[i][j] = dt0;
     }
   }

   dtp = malloc(sizeof(DT1)*DIM1*DIM2);
   dtp = stsum(&dta[0][0],&dta[0][0]);

   dtp = fun2(dtp,&dtb[0][0]);

   for ( k = 0; k < DIM2; k++ ) {
    for ( l = 0; l < DIM1; l++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
       if ( dtb[k][l].var_a[i][j] != createcomplexf((float)(2*(i*DIM2+j+1)+1),(float)(2*(i*DIM2+j+1)+1)) ) exit(53);
       if ( dtb[k][l].var_b[i][j] != 2*(i*DIM2+j+2)+1 ) exit(55);
       if ( dtb[k][l].var_c[i][j] != createcomplexl((long double)(2*(i*DIM2+j+3)+1),(long double)(2*(i*DIM2+j+3)+1)) ) exit(57);
       if ( dtb[k][l].var_d[i][j] != createcomplex((double)(2*(i*DIM2+j+4)+1),(double)(2*(i*DIM2+j+4)+1)) ) exit(59);
       if ( dtb[k][l].var_e[i][j] != createcomplexf((float)(2*(i*DIM2+j+5)+1),(float)(2*(i*DIM2+j+5)+1)) ) exit(61);
       if ( dtb[k][l].var_f[i][j] != 2*(i*DIM2+j+6)+1 ) exit(63);
       if ( dtb[k][l].var_g[i][j] != createcomplex((double)(2*(i*DIM2+j+7)+1),(double)(2*(i*DIM2+j+7)+1)) ) exit(65);
       if ( dtb[k][l].var_h[i][j] != 2*(i*DIM2+j+8)+1 ) exit(67);
      }
     }
    }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
       if ( (dtp+k)->var_a[i][j] != createcomplexf((float)(2*(i*DIM2+j+1)+1),(float)(2*(i*DIM2+j+1)+1)) ) exit(69);
       if ( (dtp+k)->var_b[i][j] != 2*(i*DIM2+j+2)+1 ) exit(71);
       if ( (dtp+k)->var_c[i][j] != createcomplexl((long double)(2*(i*DIM2+j+3)+1),(long double)(2*(i*DIM2+j+3)+1)) ) exit(73);
       if ( (dtp+k)->var_d[i][j] != createcomplex((double)(2*(i*DIM2+j+4)+1),(double)(2*(i*DIM2+j+4)+1)) ) exit(75);
       if ( (dtp+k)->var_e[i][j] != createcomplexf((float)(2*(i*DIM2+j+5)+1),(float)(2*(i*DIM2+j+5)+1)) ) exit(77);
       if ( (dtp+k)->var_f[i][j] != 2*(i*DIM2+j+6)+1 ) exit(79);
       if ( (dtp+k)->var_g[i][j] != createcomplex((double)(2*(i*DIM2+j+7)+1),(double)(2*(i*DIM2+j+7)+1)) ) exit(81);
       if ( (dtp+k)->var_h[i][j] != 2*(i*DIM2+j+8)+1 ) exit(83);
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
