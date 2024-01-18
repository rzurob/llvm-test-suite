
#include <stdio.h>
#include <stdlib.h>

#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

typedef struct dt1 DT1;

#define DIM1 2
#define DIM2 3

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

void sub1(DT1 *dt) {
   int i, j, k;

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
       if ( (dt+k)->var_a[i][j] != (float)(i*DIM2+j+1)+I*(float)(i*DIM2+j+1) ) exit(21);
#else
       if ( (dt+k)->var_a[i][j] != createcomplexf((float)(i*DIM2+j+1),(float)(i*DIM2+j+1)) ) exit(21);
#endif
       if ( (dt+k)->var_b[i][j] != i*DIM2+j+2 ) exit(23);
#ifdef CMPLX
       if ( (dt+k)->var_c[i][j] != (long double)(i*DIM2+j+3)+I*(long double)(i*DIM2+j+3) ) exit(25);
#else
       if ( (dt+k)->var_c[i][j] != createcomplexl((long double)(i*DIM2+j+3),(long double)(i*DIM2+j+3)) ) exit(25);
#endif
#ifdef CMPLX
       if ( (dt+k)->var_d[i][j] != (double)(i*DIM2+j+4)+I*(double)(i*DIM2+j+4) ) exit(27);
#else
       if ( (dt+k)->var_d[i][j] != createcomplex((double)(i*DIM2+j+4),(double)(i*DIM2+j+4)) ) exit(27);
#endif
#ifdef CMPLX
       if ( (dt+k)->var_e[i][j] != (float)(i*DIM2+j+5)+I*(float)(i*DIM2+j+5) ) exit(29);
#else
       if ( (dt+k)->var_e[i][j] != createcomplexf((float)(i*DIM2+j+5),(float)(i*DIM2+j+5)) ) exit(29);
#endif
       if ( (dt+k)->var_f[i][j] != i*DIM2+j+6 ) exit(31);
#ifdef CMPLX
       if ( (dt+k)->var_g[i][j] != (double)(i*DIM2+j+7)+I*(double)(i*DIM2+j+7) ) exit(33);
#else
       if ( (dt+k)->var_g[i][j] != createcomplex((double)(i*DIM2+j+7),(double)(i*DIM2+j+7)) ) exit(33);
#endif
       if ( (dt+k)->var_h[i][j] != i*DIM2+j+8 ) exit(35);
     }
    }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
       (dt+k)->var_a[i][j] += 1.0f+I*1.0f;
#else
       (dt+k)->var_a[i][j] += createcomplexf(1.0f,1.0f);
#endif
       (dt+k)->var_b[i][j] += 1;
#ifdef CMPLX
       (dt+k)->var_c[i][j] += 1.0l+I*1.0l;
#else
       (dt+k)->var_c[i][j] += createcomplexl(1.0l,1.0l);
#endif
#ifdef CMPLX
       (dt+k)->var_d[i][j] += 1.0+I*1.0;
#else
       (dt+k)->var_d[i][j] += createcomplex(1.0,1.0);
#endif
#ifdef CMPLX
       (dt+k)->var_e[i][j] += 1.0f+I*1.0f;
#else
       (dt+k)->var_e[i][j] += createcomplexf(1.0f,1.0f);
#endif
       (dt+k)->var_f[i][j] += 1;
#ifdef CMPLX
       (dt+k)->var_g[i][j] += 1.0+I*1.0;
#else
       (dt+k)->var_g[i][j] += createcomplex(1.0,1.0);
#endif
       (dt+k)->var_h[i][j] += 1;
     }
    }
   }

}

void sub2(DT1 *dtx, DT1 *dty) {
   int i, j, k;

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
       if ( (dtx+k)->var_a[i][j] != (float)(2*(i*DIM2+j+1))+I*(float)(2*(i*DIM2+j+1)) ) exit(37);
#else
       if ( (dtx+k)->var_a[i][j] != createcomplexf((float)(2*(i*DIM2+j+1)),(float)(2*(i*DIM2+j+1))) ) exit(37);
#endif
       if ( (dtx+k)->var_b[i][j] != 2*(i*DIM2+j+2) ) exit(39);
#ifdef CMPLX
       if ( (dtx+k)->var_c[i][j] != (long double)(2*(i*DIM2+j+3))+I*(long double)(2*(i*DIM2+j+3)) ) exit(41);
#else
       if ( (dtx+k)->var_c[i][j] != createcomplexl((long double)(2*(i*DIM2+j+3)),(long double)(2*(i*DIM2+j+3))) ) exit(41);
#endif
#ifdef CMPLX
       if ( (dtx+k)->var_d[i][j] != (double)(2*(i*DIM2+j+4))+I*(double)(2*(i*DIM2+j+4)) ) exit(43);
#else
       if ( (dtx+k)->var_d[i][j] != createcomplex((double)(2*(i*DIM2+j+4)),(double)(2*(i*DIM2+j+4))) ) exit(43);
#endif
#ifdef CMPLX
       if ( (dtx+k)->var_e[i][j] != (float)(2*(i*DIM2+j+5))+I*(float)(2*(i*DIM2+j+5)) ) exit(45);
#else
       if ( (dtx+k)->var_e[i][j] != createcomplexf((float)(2*(i*DIM2+j+5)),(float)(2*(i*DIM2+j+5))) ) exit(45);
#endif
       if ( (dtx+k)->var_f[i][j] != 2*(i*DIM2+j+6) ) exit(47);
#ifdef CMPLX
       if ( (dtx+k)->var_g[i][j] != (double)(2*(i*DIM2+j+7))+I*(double)(2*(i*DIM2+j+7)) ) exit(49);
#else
       if ( (dtx+k)->var_g[i][j] != createcomplex((double)(2*(i*DIM2+j+7)),(double)(2*(i*DIM2+j+7))) ) exit(49);
#endif
       if ( (dtx+k)->var_h[i][j] != 2*(i*DIM2+j+8) ) exit(51);
     }
    }
   }

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
       (dty+k)->var_a[i][j] = (dtx+k)->var_a[i][j] + 1.0f+I*1.0f;
#else
       (dty+k)->var_a[i][j] = (dtx+k)->var_a[i][j] + createcomplexf(1.0f,1.0f);
#endif
       (dty+k)->var_b[i][j] = (dtx+k)->var_b[i][j] + 1;
#ifdef CMPLX
       (dty+k)->var_c[i][j] = (dtx+k)->var_c[i][j] + 1.0l+I*1.0l;
#else
       (dty+k)->var_c[i][j] = (dtx+k)->var_c[i][j] + createcomplexl(1.0l,1.0l);
#endif
#ifdef CMPLX
       (dty+k)->var_d[i][j] = (dtx+k)->var_d[i][j] + 1.0+I*1.0;
#else
       (dty+k)->var_d[i][j] = (dtx+k)->var_d[i][j] + createcomplex(1.0,1.0);
#endif
#ifdef CMPLX
       (dty+k)->var_e[i][j] = (dtx+k)->var_e[i][j] + 1.0f+I*1.0f;
#else
       (dty+k)->var_e[i][j] = (dtx+k)->var_e[i][j] + createcomplexf(1.0f,1.0f);
#endif
       (dty+k)->var_f[i][j] = (dtx+k)->var_f[i][j] + 1;
#ifdef CMPLX
       (dty+k)->var_g[i][j] = (dtx+k)->var_g[i][j] + 1.0+I*1.0;
#else
       (dty+k)->var_g[i][j] = (dtx+k)->var_g[i][j] + createcomplex(1.0,1.0);
#endif
       (dty+k)->var_h[i][j] = (dtx+k)->var_h[i][j] + 1;
     }
    }
   }
}
