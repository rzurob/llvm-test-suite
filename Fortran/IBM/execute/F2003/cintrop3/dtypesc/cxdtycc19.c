
#include <stdio.h>
#include <stdlib.h>

#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

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

#ifdef CMPLX
#define ARRDT1 {1.0f+I*1.0f,2.0f+I*2.0f,3.0f+I*3.0f,4.0f+I*4.0f,5.0f+I*5.0f,6.0f+I*6.0f}, {2,3,4,5,6,7}, {3.0l+I*3.0l,4.0l+I*4.0l,5.0l+I*5.0l,6.0l+I*6.0l,7.0l+I*7.0l,8.0l+I*8.0l}, {4.0+I*4.0,5.0+I*5.0,6.0+I*6.0,7.0+I*7.0,8.0+I*8.0,9.0+I*9.0}, {5.0f+I*5.0f,6.0f+I*6.0f,7.0f+I*7.0f,8.0f+I*8.0f,9.0f+I*9.0f,10.0f+I*10.0f}, {6,7,8,9,10,11}, {7.0+I*7.0,8.0+I*8.0,9.0+I*9.0,10.0+I*10.0,11.0+I*11.0,12.0+I*12.0}, {8,9,10,11,12,13}
#else
#define ARRDT1 {createcomplexf(1.0f,1.0f),createcomplexf(2.0f,2.0f),createcomplexf(3.0f,3.0f),createcomplexf(4.0f,4.0f),createcomplexf(5.0f,5.0f),createcomplexf(6.0f,6.0f)}, {2,3,4,5,6,7}, {createcomplexl(3.0l,3.0l),createcomplexl(4.0l,4.0l),createcomplexl(5.0l,5.0l),createcomplexl(6.0l,6.0l),createcomplexl(7.0l,7.0l),createcomplexl(8.0l,8.0l)}, {createcomplex(4.0,4.0),createcomplex(5.0,5.0),createcomplex(6.0,6.0),createcomplex(7.0,7.0),createcomplex(8.0,8.0),createcomplex(9.0,9.0)}, {createcomplexf(5.0f,5.0f),createcomplexf(6.0f,6.0f),createcomplexf(7.0f,7.0f),createcomplexf(8.0f,8.0f),createcomplexf(9.0f,9.0f),createcomplexf(10.0f,10.0f)}, {6,7,8,9,10,11}, {createcomplex(7.0,7.0),createcomplex(8.0,8.0),createcomplex(9.0,9.0),createcomplex(10.0,10.0),createcomplex(11.0,11.0),createcomplex(12.0,12.0)}, {8,9,10,11,12,13}
#endif

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
#ifdef CMPLX
       if ( (dta+k)->var_a[i][j] != (float)(i*DIM2+j+2)+I*(float)(i*DIM2+j+2) ) exit(21);
#else
       if ( (dta+k)->var_a[i][j] != createcomplexf((float)(i*DIM2+j+2),(float)(i*DIM2+j+2)) ) exit(21);
#endif
       if ( (dta+k)->var_b[i][j] != i*DIM2+j+3 ) exit(23);
#ifdef CMPLX
       if ( (dta+k)->var_c[i][j] != (long double)(i*DIM2+j+4)+I*(long double)(i*DIM2+j+4) ) exit(25);
#else
       if ( (dta+k)->var_c[i][j] != createcomplexl((long double)(i*DIM2+j+4),(long double)(i*DIM2+j+4)) ) exit(25);
#endif
#ifdef CMPLX
       if ( (dta+k)->var_d[i][j] != (double)(i*DIM2+j+5)+I*(double)(i*DIM2+j+5) ) exit(27);
#else
       if ( (dta+k)->var_d[i][j] != createcomplex((double)(i*DIM2+j+5),(double)(i*DIM2+j+5)) ) exit(27);
#endif
#ifdef CMPLX
       if ( (dta+k)->var_e[i][j] != (float)(i*DIM2+j+6)+I*(float)(i*DIM2+j+6) ) exit(29);
#else
       if ( (dta+k)->var_e[i][j] != createcomplexf((float)(i*DIM2+j+6),(float)(i*DIM2+j+6)) ) exit(29);
#endif
       if ( (dta+k)->var_f[i][j] != i*DIM2+j+7 ) exit(31);
#ifdef CMPLX
       if ( (dta+k)->var_g[i][j] != (double)(i*DIM2+j+8)+I*(double)(i*DIM2+j+8) ) exit(33);
#else
       if ( (dta+k)->var_g[i][j] != createcomplex((double)(i*DIM2+j+8),(double)(i*DIM2+j+8)) ) exit(33);
#endif
       if ( (dta+k)->var_h[i][j] != i*DIM2+j+9 ) exit(35);
      }
     }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
       if ( (dtp+k)->var_a[i][j] != (float)(i*DIM2+j+2)+I*(float)(i*DIM2+j+2) ) exit(37);
#else
       if ( (dtp+k)->var_a[i][j] != createcomplexf((float)(i*DIM2+j+2),(float)(i*DIM2+j+2)) ) exit(37);
#endif
       if ( (dtp+k)->var_b[i][j] != i*DIM2+j+3 ) exit(39);
#ifdef CMPLX
       if ( (dtp+k)->var_c[i][j] != (long double)(i*DIM2+j+4)+I*(long double)(i*DIM2+j+4) ) exit(41);
#else
       if ( (dtp+k)->var_c[i][j] != createcomplexl((long double)(i*DIM2+j+4),(long double)(i*DIM2+j+4)) ) exit(41);
#endif
#ifdef CMPLX
       if ( (dtp+k)->var_d[i][j] != (double)(i*DIM2+j+5)+I*(double)(i*DIM2+j+5) ) exit(43);
#else
       if ( (dtp+k)->var_d[i][j] != createcomplex((double)(i*DIM2+j+5),(double)(i*DIM2+j+5)) ) exit(43);
#endif
#ifdef CMPLX
       if ( (dtp+k)->var_e[i][j] != (float)(i*DIM2+j+6)+I*(float)(i*DIM2+j+6) ) exit(45);
#else
       if ( (dtp+k)->var_e[i][j] != createcomplexf((float)(i*DIM2+j+6),(float)(i*DIM2+j+6)) ) exit(45);
#endif
       if ( (dtp+k)->var_f[i][j] != i*DIM2+j+7 ) exit(47);
#ifdef CMPLX
       if ( (dtp+k)->var_g[i][j] != (double)(i*DIM2+j+8)+I*(double)(i*DIM2+j+8) ) exit(49);
#else
       if ( (dtp+k)->var_g[i][j] != createcomplex((double)(i*DIM2+j+8),(double)(i*DIM2+j+8)) ) exit(49);
#endif
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
#ifdef CMPLX
       if ( (dtb+k)->var_a[i][j] != (float)(2*(i*DIM2+j+1)+1)+I*(float)(2*(i*DIM2+j+1)+1) ) exit(53);
#else
       if ( (dtb+k)->var_a[i][j] != createcomplexf((float)(2*(i*DIM2+j+1)+1),(float)(2*(i*DIM2+j+1)+1)) ) exit(53);
#endif
       if ( (dtb+k)->var_b[i][j] != 2*(i*DIM2+j+2)+1 ) exit(55);
#ifdef CMPLX
       if ( (dtb+k)->var_c[i][j] != (long double)(2*(i*DIM2+j+3)+1)+I*(long double)(2*(i*DIM2+j+3)+1) ) exit(57);
#else
       if ( (dtb+k)->var_c[i][j] != createcomplexl((long double)(2*(i*DIM2+j+3)+1),(long double)(2*(i*DIM2+j+3)+1)) ) exit(57);
#endif
#ifdef CMPLX
       if ( (dtb+k)->var_d[i][j] != (double)(2*(i*DIM2+j+4)+1)+I*(double)(2*(i*DIM2+j+4)+1) ) exit(59);
#else
       if ( (dtb+k)->var_d[i][j] != createcomplex((double)(2*(i*DIM2+j+4)+1),(double)(2*(i*DIM2+j+4)+1)) ) exit(59);
#endif
#ifdef CMPLX
       if ( (dtb+k)->var_e[i][j] != (float)(2*(i*DIM2+j+5)+1)+I*(float)(2*(i*DIM2+j+5)+1) ) exit(61);
#else
       if ( (dtb+k)->var_e[i][j] != createcomplexf((float)(2*(i*DIM2+j+5)+1),(float)(2*(i*DIM2+j+5)+1)) ) exit(61);
#endif
       if ( (dtb+k)->var_f[i][j] != 2*(i*DIM2+j+6)+1 ) exit(63);
#ifdef CMPLX
       if ( (dtb+k)->var_g[i][j] != (double)(2*(i*DIM2+j+7)+1)+I*(double)(2*(i*DIM2+j+7)+1) ) exit(65);
#else
       if ( (dtb+k)->var_g[i][j] != createcomplex((double)(2*(i*DIM2+j+7)+1),(double)(2*(i*DIM2+j+7)+1)) ) exit(65);
#endif
       if ( (dtb+k)->var_h[i][j] != 2*(i*DIM2+j+8)+1 ) exit(67);
      }
     }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
       if ( (dtp+k)->var_a[i][j] != (float)(2*(i*DIM2+j+1)+1)+I*(float)(2*(i*DIM2+j+1)+1) ) exit(69);
#else
       if ( (dtp+k)->var_a[i][j] != createcomplexf((float)(2*(i*DIM2+j+1)+1),(float)(2*(i*DIM2+j+1)+1)) ) exit(69);
#endif
       if ( (dtp+k)->var_b[i][j] != 2*(i*DIM2+j+2)+1 ) exit(71);
#ifdef CMPLX
       if ( (dtp+k)->var_c[i][j] != (long double)(2*(i*DIM2+j+3)+1)+I*(long double)(2*(i*DIM2+j+3)+1) ) exit(73);
#else
       if ( (dtp+k)->var_c[i][j] != createcomplexl((long double)(2*(i*DIM2+j+3)+1),(long double)(2*(i*DIM2+j+3)+1)) ) exit(73);
#endif
#ifdef CMPLX
       if ( (dtp+k)->var_d[i][j] != (double)(2*(i*DIM2+j+4)+1)+I*(double)(2*(i*DIM2+j+4)+1) ) exit(75);
#else
       if ( (dtp+k)->var_d[i][j] != createcomplex((double)(2*(i*DIM2+j+4)+1),(double)(2*(i*DIM2+j+4)+1)) ) exit(75);
#endif
#ifdef CMPLX
       if ( (dtp+k)->var_e[i][j] != (float)(2*(i*DIM2+j+5)+1)+I*(float)(2*(i*DIM2+j+5)+1) ) exit(77);
#else
       if ( (dtp+k)->var_e[i][j] != createcomplexf((float)(2*(i*DIM2+j+5)+1),(float)(2*(i*DIM2+j+5)+1)) ) exit(77);
#endif
       if ( (dtp+k)->var_f[i][j] != 2*(i*DIM2+j+6)+1 ) exit(79);
#ifdef CMPLX
       if ( (dtp+k)->var_g[i][j] != (double)(2*(i*DIM2+j+7)+1)+I*(double)(2*(i*DIM2+j+7)+1) ) exit(81);
#else
       if ( (dtp+k)->var_g[i][j] != createcomplex((double)(2*(i*DIM2+j+7)+1),(double)(2*(i*DIM2+j+7)+1)) ) exit(81);
#endif
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