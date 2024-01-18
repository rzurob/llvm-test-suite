
#include <stdio.h>
#include <stdlib.h>

#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

#include <inttypes.h>
#include <stddef.h>

#if ( defined(_AIX) && ! defined(_AIX52) )
  #define int_fast16_t short
#endif

typedef struct dt1 DT1;
typedef struct dt2 DT2;
typedef struct dt3 DT3;

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
   double _Complex var_a[DIM1][DIM2];
   int var_b[DIM1][DIM2];
   double _Complex var_c[DIM1][DIM2];
   char var_d[DIM1][DIM2];
   long double _Complex var_e[DIM1][DIM2];
   float _Complex var_f[DIM1][DIM2];
   int32_t var_g[DIM1][DIM2];
   float _Complex var_h[DIM1][DIM2];
   DT1 vdt1[DIM2][DIM1];
};

struct dt3 {
   DT2 vdt2[DIM2][DIM1];
   float _Complex var_a[DIM1][DIM2];
   short var_b[DIM1][DIM2];
   long double _Complex var_c[DIM1][DIM2];
   double _Complex var_d[DIM1][DIM2];
   float _Complex var_e[DIM1][DIM2];
   char var_f[DIM1][DIM2];
   double _Complex var_g[DIM1][DIM2];
   int var_h[DIM1][DIM2];
};

DT3 *fun1(DT3 *dt) {
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

#ifdef CMPLX
       if ( (dt+k)->vdt2[j][i].var_a[i][j] != (double)(i*DIM2+j+1)+I*(double)(i*DIM2+j+1) ) exit(37);
#else
       if ( (dt+k)->vdt2[j][i].var_a[i][j] != createcomplex((double)(i*DIM2+j+1),(double)(i*DIM2+j+1)) ) exit(37);
#endif
       if ( (dt+k)->vdt2[j][i].var_b[i][j] != i*DIM2+j+2 ) exit(39);
#ifdef CMPLX
       if ( (dt+k)->vdt2[j][i].var_c[i][j] != (double)(i*DIM2+j+3)+I*(double)(i*DIM2+j+3) ) exit(41);
#else
       if ( (dt+k)->vdt2[j][i].var_c[i][j] != createcomplex((double)(i*DIM2+j+3),(double)(i*DIM2+j+3)) ) exit(41);
#endif
       if ( (dt+k)->vdt2[j][i].var_d[i][j] != i*DIM2+j+4 ) exit(43);
#ifdef CMPLX
       if ( (dt+k)->vdt2[j][i].var_e[i][j] != (long double)(i*DIM2+j+5)+I*(long double)(i*DIM2+j+5) ) exit(45);
#else
       if ( (dt+k)->vdt2[j][i].var_e[i][j] != createcomplexl((long double)(i*DIM2+j+5),(long double)(i*DIM2+j+5)) ) exit(45);
#endif
#ifdef CMPLX
       if ( (dt+k)->vdt2[j][i].var_f[i][j] != (float)(i*DIM2+j+6)+I*(float)(i*DIM2+j+6) ) exit(47);
#else
       if ( (dt+k)->vdt2[j][i].var_f[i][j] != createcomplexf((float)(i*DIM2+j+6),(float)(i*DIM2+j+6)) ) exit(47);
#endif
       if ( (dt+k)->vdt2[j][i].var_g[i][j] != i*DIM2+j+7 ) exit(49);
#ifdef CMPLX
       if ( (dt+k)->vdt2[j][i].var_h[i][j] != (float)(i*DIM2+j+8)+I*(float)(i*DIM2+j+8) ) exit(51);
#else
       if ( (dt+k)->vdt2[j][i].var_h[i][j] != createcomplexf((float)(i*DIM2+j+8),(float)(i*DIM2+j+8)) ) exit(51);
#endif

       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_a[i][j] != i*DIM2+j+1 ) exit(53);
#ifdef CMPLX
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] != (double)(i*DIM2+j+2)+I*(double)(i*DIM2+j+2) ) exit(55);
#else
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] != createcomplex((double)(i*DIM2+j+2),(double)(i*DIM2+j+2)) ) exit(55);
#endif
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_c[i][j] != i*DIM2+j+3 ) exit(57);
#ifdef CMPLX
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] != (float)(i*DIM2+j+4)+I*(float)(i*DIM2+j+4) ) exit(59);
#else
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] != createcomplexf((float)(i*DIM2+j+4),(float)(i*DIM2+j+4)) ) exit(59);
#endif
#ifdef CMPLX
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] != (long double)(i*DIM2+j+5)+I*(long double)(i*DIM2+j+5) ) exit(61);
#else
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] != createcomplexl((long double)(i*DIM2+j+5),(long double)(i*DIM2+j+5)) ) exit(61);
#endif
#ifdef CMPLX
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] != (double)(i*DIM2+j+6)+I*(double)(i*DIM2+j+6) ) exit(63);
#else
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] != createcomplex((double)(i*DIM2+j+6),(double)(i*DIM2+j+6)) ) exit(63);
#endif
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_g[i][j] != i*DIM2+j+7 ) exit(65);
#ifdef CMPLX
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] != (float)(i*DIM2+j+8)+I*(float)(i*DIM2+j+8) ) exit(67);
#else
       if ( (dt+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] != createcomplexf((float)(i*DIM2+j+8),(float)(i*DIM2+j+8)) ) exit(67);
#endif
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

#ifdef CMPLX
       (dt+k)->vdt2[j][i].var_a[i][j] += 2.0+I*2.0;
#else
       (dt+k)->vdt2[j][i].var_a[i][j] += createcomplex(2.0,2.0);
#endif
       (dt+k)->vdt2[j][i].var_b[i][j] += 2;
#ifdef CMPLX
       (dt+k)->vdt2[j][i].var_c[i][j] += 2.0+I*2.0;
#else
       (dt+k)->vdt2[j][i].var_c[i][j] += createcomplex(2.0,2.0);
#endif
       (dt+k)->vdt2[j][i].var_d[i][j] += 2;
#ifdef CMPLX
       (dt+k)->vdt2[j][i].var_e[i][j] += 2.0l+I*2.0l;
#else
       (dt+k)->vdt2[j][i].var_e[i][j] += createcomplexl(2.0l,2.0l);
#endif
#ifdef CMPLX
       (dt+k)->vdt2[j][i].var_f[i][j] += 2.0f+I*2.0f;
#else
       (dt+k)->vdt2[j][i].var_f[i][j] += createcomplexf(2.0f,2.0f);
#endif
       (dt+k)->vdt2[j][i].var_g[i][j] += 2;
#ifdef CMPLX
       (dt+k)->vdt2[j][i].var_h[i][j] += 2.0f+I*2.0f;
#else
       (dt+k)->vdt2[j][i].var_h[i][j] += createcomplexf(2.0f,2.0f);
#endif

       (dt+k)->vdt2[j][i].vdt1[j][i].var_a[i][j] += 3;
#ifdef CMPLX
       (dt+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] += 3.0+I*3.0;
#else
       (dt+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] += createcomplex(3.0,3.0);
#endif
       (dt+k)->vdt2[j][i].vdt1[j][i].var_c[i][j] += 3;
#ifdef CMPLX
       (dt+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] += 3.0f+I*3.0f;
#else
       (dt+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] += createcomplexf(3.0f,3.0f);
#endif
#ifdef CMPLX
       (dt+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] += 3.0l+I*3.0l;
#else
       (dt+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] += createcomplexl(3.0l,3.0l);
#endif
#ifdef CMPLX
       (dt+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] += 3.0+I*3.0;
#else
       (dt+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] += createcomplex(3.0,3.0);
#endif
       (dt+k)->vdt2[j][i].vdt1[j][i].var_g[i][j] += 3;
#ifdef CMPLX
       (dt+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] += 3.0f+I*3.0f;
#else
       (dt+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] += createcomplexf(3.0f,3.0f);
#endif
     }
    }
   }
   return(dt);
}

DT3 *fun2(DT3 *dtx, DT3 *dty) {
   int i, j, k;

   DT3 *dtz;

   dtz = malloc(sizeof(DT3)*DIM1*DIM2);

   for ( k = 0; k < DIM1*DIM2; k++ ) {
    for ( i = 0; i < DIM1; i++ ) {
     for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
       if ( (dtx+k)->var_a[i][j] != (float)(2*(i*DIM2+j+1))+I*(float)(2*(i*DIM2+j+1)) ) exit(69);
#else
       if ( (dtx+k)->var_a[i][j] != createcomplexf((float)(2*(i*DIM2+j+1)),(float)(2*(i*DIM2+j+1))) ) exit(69);
#endif
       if ( (dtx+k)->var_b[i][j] != 2*(i*DIM2+j+2) ) exit(71);
#ifdef CMPLX
       if ( (dtx+k)->var_c[i][j] != (long double)(2*(i*DIM2+j+3))+I*(long double)(2*(i*DIM2+j+3)) ) exit(73);
#else
       if ( (dtx+k)->var_c[i][j] != createcomplexl((long double)(2*(i*DIM2+j+3)),(long double)(2*(i*DIM2+j+3))) ) exit(73);
#endif
#ifdef CMPLX
       if ( (dtx+k)->var_d[i][j] != (double)(2*(i*DIM2+j+4))+I*(double)(2*(i*DIM2+j+4)) ) exit(75);
#else
       if ( (dtx+k)->var_d[i][j] != createcomplex((double)(2*(i*DIM2+j+4)),(double)(2*(i*DIM2+j+4))) ) exit(75);
#endif
#ifdef CMPLX
       if ( (dtx+k)->var_e[i][j] != (float)(2*(i*DIM2+j+5))+I*(float)(2*(i*DIM2+j+5)) ) exit(77);
#else
       if ( (dtx+k)->var_e[i][j] != createcomplexf((float)(2*(i*DIM2+j+5)),(float)(2*(i*DIM2+j+5))) ) exit(77);
#endif
       if ( (dtx+k)->var_f[i][j] != 2*(i*DIM2+j+6) ) exit(79);
#ifdef CMPLX
       if ( (dtx+k)->var_g[i][j] != (double)(2*(i*DIM2+j+7))+I*(double)(2*(i*DIM2+j+7)) ) exit(81);
#else
       if ( (dtx+k)->var_g[i][j] != createcomplex((double)(2*(i*DIM2+j+7)),(double)(2*(i*DIM2+j+7))) ) exit(81);
#endif
       if ( (dtx+k)->var_h[i][j] != 2*(i*DIM2+j+8) ) exit(83);

#ifdef CMPLX
       if ( (dtx+k)->vdt2[j][i].var_a[i][j] != (double)(2*(i*DIM2+j+1))+I*(double)(2*(i*DIM2+j+1)) ) exit(85);
#else
       if ( (dtx+k)->vdt2[j][i].var_a[i][j] != createcomplex((double)(2*(i*DIM2+j+1)),(double)(2*(i*DIM2+j+1))) ) exit(85);
#endif
       if ( (dtx+k)->vdt2[j][i].var_b[i][j] != 2*(i*DIM2+j+2) ) exit(87);
#ifdef CMPLX
       if ( (dtx+k)->vdt2[j][i].var_c[i][j] != (double)(2*(i*DIM2+j+3))+I*(double)(2*(i*DIM2+j+3)) ) exit(89);
#else
       if ( (dtx+k)->vdt2[j][i].var_c[i][j] != createcomplex((double)(2*(i*DIM2+j+3)),(double)(2*(i*DIM2+j+3))) ) exit(89);
#endif
       if ( (dtx+k)->vdt2[j][i].var_d[i][j] != 2*(i*DIM2+j+4) ) exit(91);
#ifdef CMPLX
       if ( (dtx+k)->vdt2[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+5))+I*(long double)(2*(i*DIM2+j+5)) ) exit(93);
#else
       if ( (dtx+k)->vdt2[j][i].var_e[i][j] != createcomplexl((long double)(2*(i*DIM2+j+5)),(long double)(2*(i*DIM2+j+5))) ) exit(93);
#endif
#ifdef CMPLX
       if ( (dtx+k)->vdt2[j][i].var_f[i][j] != (float)(2*(i*DIM2+j+6))+I*(float)(2*(i*DIM2+j+6)) ) exit(95);
#else
       if ( (dtx+k)->vdt2[j][i].var_f[i][j] != createcomplexf((float)(2*(i*DIM2+j+6)),(float)(2*(i*DIM2+j+6))) ) exit(95);
#endif
       if ( (dtx+k)->vdt2[j][i].var_g[i][j] != 2*(i*DIM2+j+7) ) exit(97);
#ifdef CMPLX
       if ( (dtx+k)->vdt2[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+8))+I*(float)(2*(i*DIM2+j+8)) ) exit(99);
#else
       if ( (dtx+k)->vdt2[j][i].var_h[i][j] != createcomplexf((float)(2*(i*DIM2+j+8)),(float)(2*(i*DIM2+j+8))) ) exit(99);
#endif

       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_a[i][j] != 2*(i*DIM2+j+1) ) exit(101);
#ifdef CMPLX
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] != (double)(2*(i*DIM2+j+2))+I*(double)(2*(i*DIM2+j+2)) ) exit(103);
#else
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] != createcomplex((double)(2*(i*DIM2+j+2)),(double)(2*(i*DIM2+j+2))) ) exit(103);
#endif
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_c[i][j] != 2*(i*DIM2+j+3) ) exit(105);
#ifdef CMPLX
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] != (float)(2*(i*DIM2+j+4))+I*(float)(2*(i*DIM2+j+4)) ) exit(107);
#else
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] != createcomplexf((float)(2*(i*DIM2+j+4)),(float)(2*(i*DIM2+j+4))) ) exit(107);
#endif
#ifdef CMPLX
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+5))+I*(long double)(2*(i*DIM2+j+5)) ) exit(109);
#else
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] != createcomplexl((long double)(2*(i*DIM2+j+5)),(long double)(2*(i*DIM2+j+5))) ) exit(109);
#endif
#ifdef CMPLX
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] != (double)(2*(i*DIM2+j+6))+I*(double)(2*(i*DIM2+j+6)) ) exit(111);
#else
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] != createcomplex((double)(2*(i*DIM2+j+6)),(double)(2*(i*DIM2+j+6))) ) exit(111);
#endif
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_g[i][j] != 2*(i*DIM2+j+7) ) exit(113);
#ifdef CMPLX
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+8))+I*(float)(2*(i*DIM2+j+8)) ) exit(115);
#else
       if ( (dtx+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] != createcomplexf((float)(2*(i*DIM2+j+8)),(float)(2*(i*DIM2+j+8))) ) exit(115);
#endif
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

#ifdef CMPLX
       (dty+k)->vdt2[j][i].var_a[i][j] = (dtx+k)->vdt2[j][i].var_a[i][j] + 2.0+I*2.0;
#else
       (dty+k)->vdt2[j][i].var_a[i][j] = (dtx+k)->vdt2[j][i].var_a[i][j] + createcomplex(2.0,2.0);
#endif
       (dty+k)->vdt2[j][i].var_b[i][j] = (dtx+k)->vdt2[j][i].var_b[i][j] + 2;
#ifdef CMPLX
       (dty+k)->vdt2[j][i].var_c[i][j] = (dtx+k)->vdt2[j][i].var_c[i][j] + 2.0+I*2.0;
#else
       (dty+k)->vdt2[j][i].var_c[i][j] = (dtx+k)->vdt2[j][i].var_c[i][j] + createcomplex(2.0,2.0);
#endif
       (dty+k)->vdt2[j][i].var_d[i][j] = (dtx+k)->vdt2[j][i].var_d[i][j] + 2;
#ifdef CMPLX
       (dty+k)->vdt2[j][i].var_e[i][j] = (dtx+k)->vdt2[j][i].var_e[i][j] + 2.0l+I*2.0l;
#else
       (dty+k)->vdt2[j][i].var_e[i][j] = (dtx+k)->vdt2[j][i].var_e[i][j] + createcomplexl(2.0l,2.0l);
#endif
#ifdef CMPLX
       (dty+k)->vdt2[j][i].var_f[i][j] = (dtx+k)->vdt2[j][i].var_f[i][j] + 2.0f+I*2.0f;
#else
       (dty+k)->vdt2[j][i].var_f[i][j] = (dtx+k)->vdt2[j][i].var_f[i][j] + createcomplexf(2.0f,2.0f);
#endif
       (dty+k)->vdt2[j][i].var_g[i][j] = (dtx+k)->vdt2[j][i].var_g[i][j] + 2;
#ifdef CMPLX
       (dty+k)->vdt2[j][i].var_h[i][j] = (dtx+k)->vdt2[j][i].var_h[i][j] + 2.0f+I*2.0f;
#else
       (dty+k)->vdt2[j][i].var_h[i][j] = (dtx+k)->vdt2[j][i].var_h[i][j] + createcomplexf(2.0f,2.0f);
#endif

       (dty+k)->vdt2[j][i].vdt1[j][i].var_a[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_a[i][j] + 3;
#ifdef CMPLX
       (dty+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] + 3.0+I*3.0;
#else
       (dty+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_b[i][j] + createcomplex(3.0,3.0);
#endif
       (dty+k)->vdt2[j][i].vdt1[j][i].var_c[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_c[i][j] + 3;
#ifdef CMPLX
       (dty+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] + 3.0f+I*3.0f;
#else
       (dty+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_d[i][j] + createcomplexf(3.0f,3.0f);
#endif
#ifdef CMPLX
       (dty+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] + 3.0l+I*3.0l;
#else
       (dty+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_e[i][j] + createcomplexl(3.0l,3.0l);
#endif
#ifdef CMPLX
       (dty+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] + 3.0+I*3.0;
#else
       (dty+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_f[i][j] + createcomplex(3.0,3.0);
#endif
       (dty+k)->vdt2[j][i].vdt1[j][i].var_g[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_g[i][j] + 3;
#ifdef CMPLX
       (dty+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] + 3.0f+I*3.0f;
#else
       (dty+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] = (dtx+k)->vdt2[j][i].vdt1[j][i].var_h[i][j] + createcomplexf(3.0f,3.0f);
#endif
     }
    }
   }
   dtz = dty;
   return(dtz);
}

