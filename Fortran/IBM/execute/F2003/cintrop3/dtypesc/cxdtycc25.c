
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

#define DIM1 2
#define DIM2 3

typedef struct dt1 DT1;
typedef struct dt2 DT2;
typedef struct dt3 DT3;

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

#ifdef CMPLX
#define ARRDT1 {1,2,3,4,5,6},{2.0+I*2.0,3.0+I*3.0,4.0+I*4.0,5.0+I*5.0,6.0+I*6.0,7.0+I*7.0},{3,4,5,6,7,8},{4.0f+I*4.0f,5.0f+I*5.0f,6.0f+I*6.0f,7.0f+I*7.0f,8.0f+I*8.0f,9.0f+I*9.0f},{5.0l+I*5.0l,6.0l+I*6.0l,7.0l+I*7.0l,8.0l+I*8.0l,9.0l+I*9.0l,10.0l+I*10.0l},{6.0+I*6.0,7.0+I*7.0,8.0+I*8.0,9.0+I*9.0,10.0+I*10.0,11.0+I*11.0},{7,8,9,10,11,12},{8.0f+I*8.0f,9.0f+I*9.0f,10.0f+I*10.0f,11.0f+I*11.0f,12.0f+I*12.0f,13.0f+I*13.0f}
#else
#define ARRDT1 {1,2,3,4,5,6},{createcomplex(2.0,2.0),createcomplex(3.0,3.0),createcomplex(4.0,4.0),createcomplex(5.0,5.0),createcomplex(6.0,6.0),createcomplex(7.0,7.0)},{3,4,5,6,7,8},{createcomplexf(4.0f,4.0f),createcomplexf(5.0f,5.0f),createcomplexf(6.0f,6.0f),createcomplexf(7.0f,7.0f),createcomplexf(8.0f,8.0f),createcomplexf(9.0f,9.0f)},{createcomplexl(5.0l,5.0l),createcomplexl(6.0l,6.0l),createcomplexl(7.0l,7.0l),createcomplexl(8.0l,8.0l),createcomplexl(9.0l,9.0l),createcomplexl(10.0l,10.0l)},{createcomplex(6.0,6.0),createcomplex(7.0,7.0),createcomplex(8.0,8.0),createcomplex(9.0,9.0),createcomplex(10.0,10.0),createcomplex(11.0,11.0)},{7,8,9,10,11,12},{createcomplexf(8.0f,8.0f),createcomplexf(9.0f,9.0f),createcomplexf(10.0f,10.0f),createcomplexf(11.0f,11.0f),createcomplexf(12.0f,12.0f),createcomplexf(13.0f,13.0f)}
#endif
#ifdef CMPLX
#define ARR2 {1.0+I*1.0,2.0+I*2.0,3.0+I*3.0,4.0+I*4.0,5.0+I*5.0,6.0+I*6.0},{2,3,4,5,6,7},{3.0+I*3.0,4.0+I*4.0,5.0+I*5.0,6.0+I*6.0,7.0+I*7.0,8.0+I*8.0},{4,5,6,7,8,9},{5.0l+I*5.0l,6.0l+I*6.0l,7.0l+I*7.0l,8.0l+I*8.0l,9.0l+I*9.0l,10.0l+I*10.0l},{6.0f+I*6.0f,7.0f+I*7.0f,8.0f+I*8.0f,9.0f+I*9.0f,10.0f+I*10.0f,11.0f+I*11.0f},{7,8,9,10,11,12},{8.0f+I*8.0f,9.0f+I*9.0f,10.0f+I*10.0f,11.0f+I*11.0f,12.0f+I*12.0f,13.0f+I*13.0f}
#else
#define ARR2 {createcomplex(1.0,1.0),createcomplex(2.0,2.0),createcomplex(3.0,3.0),createcomplex(4.0,4.0),createcomplex(5.0,5.0),createcomplex(6.0,6.0)},{2,3,4,5,6,7},{createcomplex(3.0,3.0),createcomplex(4.0,4.0),createcomplex(5.0,5.0),createcomplex(6.0,6.0),createcomplex(7.0,7.0),createcomplex(8.0,8.0)},{4,5,6,7,8,9},{createcomplexl(5.0l,5.0l),createcomplexl(6.0l,6.0l),createcomplexl(7.0l,7.0l),createcomplexl(8.0l,8.0l),createcomplexl(9.0l,9.0l),createcomplexl(10.0l,10.0l)},{createcomplexf(6.0f,6.0f),createcomplexf(7.0f,7.0f),createcomplexf(8.0f,8.0f),createcomplexf(9.0f,9.0f),createcomplexf(10.0f,10.0f),createcomplexf(11.0f,11.0f)},{7,8,9,10,11,12},{createcomplexf(8.0f,8.0f),createcomplexf(9.0f,9.0f),createcomplexf(10.0f,10.0f),createcomplexf(11.0f,11.0f),createcomplexf(12.0f,12.0f),createcomplexf(13.0f,13.0f)}
#endif
#ifdef CMPLX
#define ARR3 {1.0f+I*1.0f,2.0f+I*2.0f,3.0f+I*3.0f,4.0f+I*4.0f,5.0f+I*5.0f,6.0f+I*6.0f},{2,3,4,5,6,7},{3.0l+I*3.0l,4.0l+I*4.0l,5.0l+I*5.0l,6.0l+I*6.0l,7.0l+I*7.0l,8.0l+I*8.0l},{4.0+I*4.0,5.0+I*5.0,6.0+I*6.0,7.0+I*7.0,8.0+I*8.0,9.0+I*9.0},{5.0f+I*5.0f,6.0f+I*6.0f,7.0f+I*7.0f,8.0f+I*8.0f,9.0f+I*9.0f,10.0f+I*10.0f},{6,7,8,9,10,11},{7.0+I*7.0,8.0+I*8.0,9.0+I*9.0,10.0+I*10.0,11.0+I*11.0,12.0+I*12.0},{8,9,10,11,12,13}
#else
#define ARR3 {createcomplexf(1.0f,1.0f),createcomplexf(2.0f,2.0f),createcomplexf(3.0f,3.0f),createcomplexf(4.0f,4.0f),createcomplexf(5.0f,5.0f),createcomplexf(6.0f,6.0f)},{2,3,4,5,6,7},{createcomplexl(3.0l,3.0l),createcomplexl(4.0l,4.0l),createcomplexl(5.0l,5.0l),createcomplexl(6.0l,6.0l),createcomplexl(7.0l,7.0l),createcomplexl(8.0l,8.0l)},{createcomplex(4.0,4.0),createcomplex(5.0,5.0),createcomplex(6.0,6.0),createcomplex(7.0,7.0),createcomplex(8.0,8.0),createcomplex(9.0,9.0)},{createcomplexf(5.0f,5.0f),createcomplexf(6.0f,6.0f),createcomplexf(7.0f,7.0f),createcomplexf(8.0f,8.0f),createcomplexf(9.0f,9.0f),createcomplexf(10.0f,10.0f)},{6,7,8,9,10,11},{createcomplex(7.0,7.0),createcomplex(8.0,8.0),createcomplex(9.0,9.0),createcomplex(10.0,10.0),createcomplex(11.0,11.0),createcomplex(12.0,12.0)},{8,9,10,11,12,13}
#endif

#define ARRDT2 ARR2,{{{ARRDT1},{ARRDT1}},{{ARRDT1},{ARRDT1}},{{ARRDT1},{ARRDT1}}}

#define ARRDT3 {{{ARRDT2},{ARRDT2}},{{ARRDT2},{ARRDT2}},{{ARRDT2},{ARRDT2}}},ARR3

DT1 *st1sum(DT1 *, DT1 *);
DT2 *st2sum(DT2 *, DT2 *);
DT3 *st3sum(DT3 *, DT3 *);

int main() {

   DT3 *fun1(DT3 *);
   DT3 *fun2(DT3 *, DT3 *);

   DT3 dt0 = { ARRDT3 };

   DT3 dta[DIM2][DIM1], dtb[DIM2][DIM1];
   DT3 *dtp, *dtr;
   int i, j, k, l;

/* Test 1 */

   for ( i = 0; i < DIM2; i++ ) {
     for ( j = 0; j < DIM1; j++ ) {
       dta[i][j] = dt0;
     }
   }

   dtr = fun1(&dta[0][0]);

   for ( k = 0; k < DIM2; k++ ) {
    for ( l = 0; l < DIM1; l++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
       if ( dta[k][l].var_a[i][j] != (float)(i*DIM2+j+2)+I*(float)(i*DIM2+j+2) ) exit(21);
#else
       if ( dta[k][l].var_a[i][j] != createcomplexf((float)(i*DIM2+j+2),(float)(i*DIM2+j+2)) ) exit(21);
#endif
       if ( dta[k][l].var_b[i][j] != i*DIM2+j+3 ) exit(23);
#ifdef CMPLX
       if ( dta[k][l].var_c[i][j] != (long double)(i*DIM2+j+4)+I*(long double)(i*DIM2+j+4) ) exit(25);
#else
       if ( dta[k][l].var_c[i][j] != createcomplexl((long double)(i*DIM2+j+4),(long double)(i*DIM2+j+4)) ) exit(25);
#endif
#ifdef CMPLX
       if ( dta[k][l].var_d[i][j] != (double)(i*DIM2+j+5)+I*(double)(i*DIM2+j+5) ) exit(27);
#else
       if ( dta[k][l].var_d[i][j] != createcomplex((double)(i*DIM2+j+5),(double)(i*DIM2+j+5)) ) exit(27);
#endif
#ifdef CMPLX
       if ( dta[k][l].var_e[i][j] != (float)(i*DIM2+j+6)+I*(float)(i*DIM2+j+6) ) exit(29);
#else
       if ( dta[k][l].var_e[i][j] != createcomplexf((float)(i*DIM2+j+6),(float)(i*DIM2+j+6)) ) exit(29);
#endif
       if ( dta[k][l].var_f[i][j] != i*DIM2+j+7 ) exit(31);
#ifdef CMPLX
       if ( dta[k][l].var_g[i][j] != (double)(i*DIM2+j+8)+I*(double)(i*DIM2+j+8) ) exit(33);
#else
       if ( dta[k][l].var_g[i][j] != createcomplex((double)(i*DIM2+j+8),(double)(i*DIM2+j+8)) ) exit(33);
#endif
       if ( dta[k][l].var_h[i][j] != i*DIM2+j+9 ) exit(35);

#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_a[i][j] != (float)(i*DIM2+j+2)+I*(float)(i*DIM2+j+2) ) exit(37);
#else
       if ( (dtr+k*DIM1+l)->var_a[i][j] != createcomplexf((float)(i*DIM2+j+2),(float)(i*DIM2+j+2)) ) exit(37);
#endif
       if ( (dtr+k*DIM1+l)->var_b[i][j] != i*DIM2+j+3 ) exit(39);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_c[i][j] != (long double)(i*DIM2+j+4)+I*(long double)(i*DIM2+j+4) ) exit(41);
#else
       if ( (dtr+k*DIM1+l)->var_c[i][j] != createcomplexl((long double)(i*DIM2+j+4),(long double)(i*DIM2+j+4)) ) exit(41);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_d[i][j] != (double)(i*DIM2+j+5)+I*(double)(i*DIM2+j+5) ) exit(43);
#else
       if ( (dtr+k*DIM1+l)->var_d[i][j] != createcomplex((double)(i*DIM2+j+5),(double)(i*DIM2+j+5)) ) exit(43);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_e[i][j] != (float)(i*DIM2+j+6)+I*(float)(i*DIM2+j+6) ) exit(45);
#else
       if ( (dtr+k*DIM1+l)->var_e[i][j] != createcomplexf((float)(i*DIM2+j+6),(float)(i*DIM2+j+6)) ) exit(45);
#endif
       if ( (dtr+k*DIM1+l)->var_f[i][j] != i*DIM2+j+7 ) exit(47);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_g[i][j] != (double)(i*DIM2+j+8)+I*(double)(i*DIM2+j+8) ) exit(49);
#else
       if ( (dtr+k*DIM1+l)->var_g[i][j] != createcomplex((double)(i*DIM2+j+8),(double)(i*DIM2+j+8)) ) exit(49);
#endif
       if ( (dtr+k*DIM1+l)->var_h[i][j] != i*DIM2+j+9 ) exit(51);

#ifdef CMPLX
       if ( dta[k][l].vdt2[j][i].var_a[i][j] != (double)(i*DIM2+j+2)+I*(double)(i*DIM2+j+2) ) exit(53);
#else
       if ( dta[k][l].vdt2[j][i].var_a[i][j] != createcomplex((double)(i*DIM2+j+2),(double)(i*DIM2+j+2)) ) exit(53);
#endif
       if ( dta[k][l].vdt2[j][i].var_b[i][j] != i*DIM2+j+3 ) exit(55);
#ifdef CMPLX
       if ( dta[k][l].vdt2[j][i].var_c[i][j] != (double)(i*DIM2+j+4)+I*(double)(i*DIM2+j+4) ) exit(57);
#else
       if ( dta[k][l].vdt2[j][i].var_c[i][j] != createcomplex((double)(i*DIM2+j+4),(double)(i*DIM2+j+4)) ) exit(57);
#endif
       if ( dta[k][l].vdt2[j][i].var_d[i][j] != i*DIM2+j+5 ) exit(59);
#ifdef CMPLX
       if ( dta[k][l].vdt2[j][i].var_e[i][j] != (long double)(i*DIM2+j+6)+I*(long double)(i*DIM2+j+6) ) exit(61);
#else
       if ( dta[k][l].vdt2[j][i].var_e[i][j] != createcomplexl((long double)(i*DIM2+j+6),(long double)(i*DIM2+j+6)) ) exit(61);
#endif
#ifdef CMPLX
       if ( dta[k][l].vdt2[j][i].var_f[i][j] != (float)(i*DIM2+j+7)+I*(float)(i*DIM2+j+7) ) exit(63);
#else
       if ( dta[k][l].vdt2[j][i].var_f[i][j] != createcomplexf((float)(i*DIM2+j+7),(float)(i*DIM2+j+7)) ) exit(63);
#endif
       if ( dta[k][l].vdt2[j][i].var_g[i][j] != i*DIM2+j+8 ) exit(65);
#ifdef CMPLX
       if ( dta[k][l].vdt2[j][i].var_h[i][j] != (float)(i*DIM2+j+9)+I*(float)(i*DIM2+j+9) ) exit(67);
#else
       if ( dta[k][l].vdt2[j][i].var_h[i][j] != createcomplexf((float)(i*DIM2+j+9),(float)(i*DIM2+j+9)) ) exit(67);
#endif

#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_a[i][j] != (double)(i*DIM2+j+2)+I*(double)(i*DIM2+j+2) ) exit(69);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_a[i][j] != createcomplex((double)(i*DIM2+j+2),(double)(i*DIM2+j+2)) ) exit(69);
#endif
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_b[i][j] != i*DIM2+j+3 ) exit(71);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_c[i][j] != (double)(i*DIM2+j+4)+I*(double)(i*DIM2+j+4) ) exit(73);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_c[i][j] != createcomplex((double)(i*DIM2+j+4),(double)(i*DIM2+j+4)) ) exit(73);
#endif
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_d[i][j] != i*DIM2+j+5 ) exit(75);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_e[i][j] != (long double)(i*DIM2+j+6)+I*(long double)(i*DIM2+j+6) ) exit(77);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_e[i][j] != createcomplexl((long double)(i*DIM2+j+6),(long double)(i*DIM2+j+6)) ) exit(77);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_f[i][j] != (float)(i*DIM2+j+7)+I*(float)(i*DIM2+j+7) ) exit(79);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_f[i][j] != createcomplexf((float)(i*DIM2+j+7),(float)(i*DIM2+j+7)) ) exit(79);
#endif
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_g[i][j] != i*DIM2+j+8 ) exit(81);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_h[i][j] != (float)(i*DIM2+j+9)+I*(float)(i*DIM2+j+9) ) exit(83);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_h[i][j] != createcomplexf((float)(i*DIM2+j+9),(float)(i*DIM2+j+9)) ) exit(83);
#endif

       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_a[i][j] != i*DIM2+j+2 ) exit(85);
#ifdef CMPLX
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_b[i][j] != (double)(i*DIM2+j+3)+I*(double)(i*DIM2+j+3) ) exit(87);
#else
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_b[i][j] != createcomplex((double)(i*DIM2+j+3),(double)(i*DIM2+j+3)) ) exit(87);
#endif
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_c[i][j] != i*DIM2+j+4 ) exit(89);
#ifdef CMPLX
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_d[i][j] != (float)(i*DIM2+j+5)+I*(float)(i*DIM2+j+5) ) exit(91);
#else
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_d[i][j] != createcomplexf((float)(i*DIM2+j+5),(float)(i*DIM2+j+5)) ) exit(91);
#endif
#ifdef CMPLX
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_e[i][j] != (long double)(i*DIM2+j+6)+I*(long double)(i*DIM2+j+6) ) exit(93);
#else
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_e[i][j] != createcomplexl((long double)(i*DIM2+j+6),(long double)(i*DIM2+j+6)) ) exit(93);
#endif
#ifdef CMPLX
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_f[i][j] != (double)(i*DIM2+j+7)+I*(double)(i*DIM2+j+7) ) exit(95);
#else
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_f[i][j] != createcomplex((double)(i*DIM2+j+7),(double)(i*DIM2+j+7)) ) exit(95);
#endif
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_g[i][j] != i*DIM2+j+8 ) exit(97);
#ifdef CMPLX
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_h[i][j] != (float)(i*DIM2+j+9)+I*(float)(i*DIM2+j+9) ) exit(99);
#else
       if ( dta[k][l].vdt2[j][i].vdt1[j][i].var_h[i][j] != createcomplexf((float)(i*DIM2+j+9),(float)(i*DIM2+j+9)) ) exit(99);
#endif

       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_a[i][j] != i*DIM2+j+2 ) exit(101);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_b[i][j] != (double)(i*DIM2+j+3)+I*(double)(i*DIM2+j+3) ) exit(103);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_b[i][j] != createcomplex((double)(i*DIM2+j+3),(double)(i*DIM2+j+3)) ) exit(103);
#endif
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_c[i][j] != i*DIM2+j+4 ) exit(105);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_d[i][j] != (float)(i*DIM2+j+5)+I*(float)(i*DIM2+j+5) ) exit(107);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_d[i][j] != createcomplexf((float)(i*DIM2+j+5),(float)(i*DIM2+j+5)) ) exit(107);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_e[i][j] != (long double)(i*DIM2+j+6)+I*(long double)(i*DIM2+j+6) ) exit(109);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_e[i][j] != createcomplexl((long double)(i*DIM2+j+6),(long double)(i*DIM2+j+6)) ) exit(109);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_f[i][j] != (double)(i*DIM2+j+7)+I*(double)(i*DIM2+j+7) ) exit(111);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_f[i][j] != createcomplex((double)(i*DIM2+j+7),(double)(i*DIM2+j+7)) ) exit(111);
#endif
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_g[i][j] != i*DIM2+j+8 ) exit(113);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_h[i][j] != (float)(i*DIM2+j+9)+I*(float)(i*DIM2+j+9) ) exit(115);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_h[i][j] != createcomplexf((float)(i*DIM2+j+9),(float)(i*DIM2+j+9)) ) exit(115);
#endif
      }
     }
    }
   }

/* Test 2 */

   for ( i = 0; i < DIM2; i++ ) {
     for ( j = 0; j < DIM1; j++ ) {
       dta[i][j] = dt0;
       dtb[i][j] = dt0;
     }
   }

   dtp = malloc(sizeof(DT3)*DIM1*DIM2);
   dtp = st3sum(&dta[0][0],&dta[0][0]);

   dtr = fun2(dtp,&dtb[0][0]);

   for ( k = 0; k < DIM2; k++ ) {
    for ( l = 0; l < DIM1; l++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
       if ( dtb[k][l].var_a[i][j] != (float)(2*(i*DIM2+j+2)-1)+I*(float)(2*(i*DIM2+j+2)-1) ) exit(117);
#else
       if ( dtb[k][l].var_a[i][j] != createcomplexf((float)(2*(i*DIM2+j+2)-1),(float)(2*(i*DIM2+j+2)-1)) ) exit(117);
#endif
       if ( dtb[k][l].var_b[i][j] != 2*(i*DIM2+j+3) ) exit(119);
#ifdef CMPLX
       if ( dtb[k][l].var_c[i][j] != (long double)(2*(i*DIM2+j+4)+1)+I*(long double)(2*(i*DIM2+j+4)+1) ) exit(121);
#else
       if ( dtb[k][l].var_c[i][j] != createcomplexl((long double)(2*(i*DIM2+j+4)+1),(long double)(2*(i*DIM2+j+4)+1)) ) exit(121);
#endif
#ifdef CMPLX
       if ( dtb[k][l].var_d[i][j] != (double)(2*(i*DIM2+j+5)+2)+I*(double)(2*(i*DIM2+j+5)+2) ) exit(123);
#else
       if ( dtb[k][l].var_d[i][j] != createcomplex((double)(2*(i*DIM2+j+5)+2),(double)(2*(i*DIM2+j+5)+2)) ) exit(123);
#endif
#ifdef CMPLX
       if ( dtb[k][l].var_e[i][j] != (float)(2*(i*DIM2+j+6)+3)+I*(float)(2*(i*DIM2+j+6)+3) ) exit(125);
#else
       if ( dtb[k][l].var_e[i][j] != createcomplexf((float)(2*(i*DIM2+j+6)+3),(float)(2*(i*DIM2+j+6)+3)) ) exit(125);
#endif
       if ( dtb[k][l].var_f[i][j] != 2*(i*DIM2+j+7)+4 ) exit(127);
#ifdef CMPLX
       if ( dtb[k][l].var_g[i][j] != (double)(2*(i*DIM2+j+8)+5)+I*(double)(2*(i*DIM2+j+8)+5) ) exit(129);
#else
       if ( dtb[k][l].var_g[i][j] != createcomplex((double)(2*(i*DIM2+j+8)+5),(double)(2*(i*DIM2+j+8)+5)) ) exit(129);
#endif
       if ( dtb[k][l].var_h[i][j] != 2*(i*DIM2+j+9)+6 ) exit(131);

#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_a[i][j] != (float)(2*(i*DIM2+j+2)-1)+I*(float)(2*(i*DIM2+j+2)-1) ) exit(133);
#else
       if ( (dtr+k*DIM1+l)->var_a[i][j] != createcomplexf((float)(2*(i*DIM2+j+2)-1),(float)(2*(i*DIM2+j+2)-1)) ) exit(133);
#endif
       if ( (dtr+k*DIM1+l)->var_b[i][j] != 2*(i*DIM2+j+3) ) exit(135);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_c[i][j] != (long double)(2*(i*DIM2+j+4)+1)+I*(long double)(2*(i*DIM2+j+4)+1) ) exit(137);
#else
       if ( (dtr+k*DIM1+l)->var_c[i][j] != createcomplexl((long double)(2*(i*DIM2+j+4)+1),(long double)(2*(i*DIM2+j+4)+1)) ) exit(137);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_d[i][j] != (double)(2*(i*DIM2+j+5)+2)+I*(double)(2*(i*DIM2+j+5)+2) ) exit(139);
#else
       if ( (dtr+k*DIM1+l)->var_d[i][j] != createcomplex((double)(2*(i*DIM2+j+5)+2),(double)(2*(i*DIM2+j+5)+2)) ) exit(139);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_e[i][j] != (float)(2*(i*DIM2+j+6)+3)+I*(float)(2*(i*DIM2+j+6)+3) ) exit(141);
#else
       if ( (dtr+k*DIM1+l)->var_e[i][j] != createcomplexf((float)(2*(i*DIM2+j+6)+3),(float)(2*(i*DIM2+j+6)+3)) ) exit(141);
#endif
       if ( (dtr+k*DIM1+l)->var_f[i][j] != 2*(i*DIM2+j+7)+4 ) exit(143);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_g[i][j] != (double)(2*(i*DIM2+j+8)+5)+I*(double)(2*(i*DIM2+j+8)+5) ) exit(145);
#else
       if ( (dtr+k*DIM1+l)->var_g[i][j] != createcomplex((double)(2*(i*DIM2+j+8)+5),(double)(2*(i*DIM2+j+8)+5)) ) exit(145);
#endif
       if ( (dtr+k*DIM1+l)->var_h[i][j] != 2*(i*DIM2+j+9)+6 ) exit(147);

#ifdef CMPLX
       if ( dtb[k][l].vdt2[j][i].var_a[i][j] != (double)(2*(i*DIM2+j+2)-1)+I*(double)(2*(i*DIM2+j+2)-1) ) exit(149);
#else
       if ( dtb[k][l].vdt2[j][i].var_a[i][j] != createcomplex((double)(2*(i*DIM2+j+2)-1),(double)(2*(i*DIM2+j+2)-1)) ) exit(149);
#endif
       if ( dtb[k][l].vdt2[j][i].var_b[i][j] != 2*(i*DIM2+j+3) ) exit(151);
#ifdef CMPLX
       if ( dtb[k][l].vdt2[j][i].var_c[i][j] != (double)(2*(i*DIM2+j+4)+1)+I*(double)(2*(i*DIM2+j+4)+1) ) exit(153);
#else
       if ( dtb[k][l].vdt2[j][i].var_c[i][j] != createcomplex((double)(2*(i*DIM2+j+4)+1),(double)(2*(i*DIM2+j+4)+1)) ) exit(153);
#endif
       if ( dtb[k][l].vdt2[j][i].var_d[i][j] != 2*(i*DIM2+j+5)+2 ) exit(155);
#ifdef CMPLX
       if ( dtb[k][l].vdt2[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+6)+3)+I*(long double)(2*(i*DIM2+j+6)+3) ) exit(157);
#else
       if ( dtb[k][l].vdt2[j][i].var_e[i][j] != createcomplexl((long double)(2*(i*DIM2+j+6)+3),(long double)(2*(i*DIM2+j+6)+3)) ) exit(157);
#endif
#ifdef CMPLX
       if ( dtb[k][l].vdt2[j][i].var_f[i][j] != (float)(2*(i*DIM2+j+7)+4)+I*(float)(2*(i*DIM2+j+7)+4) ) exit(159);
#else
       if ( dtb[k][l].vdt2[j][i].var_f[i][j] != createcomplexf((float)(2*(i*DIM2+j+7)+4),(float)(2*(i*DIM2+j+7)+4)) ) exit(159);
#endif
       if ( dtb[k][l].vdt2[j][i].var_g[i][j] != 2*(i*DIM2+j+8)+5 ) exit(161);
#ifdef CMPLX
       if ( dtb[k][l].vdt2[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+9)+6)+I*(float)(2*(i*DIM2+j+9)+6) ) exit(163);
#else
       if ( dtb[k][l].vdt2[j][i].var_h[i][j] != createcomplexf((float)(2*(i*DIM2+j+9)+6),(float)(2*(i*DIM2+j+9)+6)) ) exit(163);
#endif

#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_a[i][j] != (double)(2*(i*DIM2+j+2)-1)+I*(double)(2*(i*DIM2+j+2)-1) ) exit(165);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_a[i][j] != createcomplex((double)(2*(i*DIM2+j+2)-1),(double)(2*(i*DIM2+j+2)-1)) ) exit(165);
#endif
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_b[i][j] != 2*(i*DIM2+j+3) ) exit(167);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_c[i][j] != (double)(2*(i*DIM2+j+4)+1)+I*(double)(2*(i*DIM2+j+4)+1) ) exit(169);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_c[i][j] != createcomplex((double)(2*(i*DIM2+j+4)+1),(double)(2*(i*DIM2+j+4)+1)) ) exit(169);
#endif
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_d[i][j] != 2*(i*DIM2+j+5)+2 ) exit(171);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+6)+3)+I*(long double)(2*(i*DIM2+j+6)+3) ) exit(173);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_e[i][j] != createcomplexl((long double)(2*(i*DIM2+j+6)+3),(long double)(2*(i*DIM2+j+6)+3)) ) exit(173);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_f[i][j] != (float)(2*(i*DIM2+j+7)+4)+I*(float)(2*(i*DIM2+j+7)+4) ) exit(175);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_f[i][j] != createcomplexf((float)(2*(i*DIM2+j+7)+4),(float)(2*(i*DIM2+j+7)+4)) ) exit(175);
#endif
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_g[i][j] != 2*(i*DIM2+j+8)+5 ) exit(177);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+9)+6)+I*(float)(2*(i*DIM2+j+9)+6) ) exit(179);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].var_h[i][j] != createcomplexf((float)(2*(i*DIM2+j+9)+6),(float)(2*(i*DIM2+j+9)+6)) ) exit(179);
#endif

       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_a[i][j] != 2*(i*DIM2+j+2)-1 ) exit(181);
#ifdef CMPLX
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_b[i][j] != (double)(2*(i*DIM2+j+3))+I*(double)(2*(i*DIM2+j+3)) ) exit(183);
#else
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_b[i][j] != createcomplex((double)(2*(i*DIM2+j+3)),(double)(2*(i*DIM2+j+3))) ) exit(183);
#endif
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_c[i][j] != 2*(i*DIM2+j+4)+1 ) exit(185);
#ifdef CMPLX
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_d[i][j] != (float)(2*(i*DIM2+j+5)+2)+I*(float)(2*(i*DIM2+j+5)+2) ) exit(187);
#else
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_d[i][j] != createcomplexf((float)(2*(i*DIM2+j+5)+2),(float)(2*(i*DIM2+j+5)+2)) ) exit(187);
#endif
#ifdef CMPLX
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+6)+3)+I*(long double)(2*(i*DIM2+j+6)+3) ) exit(189);
#else
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_e[i][j] != createcomplexl((long double)(2*(i*DIM2+j+6)+3),(long double)(2*(i*DIM2+j+6)+3)) ) exit(189);
#endif
#ifdef CMPLX
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_f[i][j] != (double)(2*(i*DIM2+j+7)+4)+I*(double)(2*(i*DIM2+j+7)+4) ) exit(191);
#else
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_f[i][j] != createcomplex((double)(2*(i*DIM2+j+7)+4),(double)(2*(i*DIM2+j+7)+4)) ) exit(191);
#endif
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_g[i][j] != 2*(i*DIM2+j+8)+5 ) exit(193);
#ifdef CMPLX
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+9)+6)+I*(float)(2*(i*DIM2+j+9)+6) ) exit(195);
#else
       if ( dtb[k][l].vdt2[j][i].vdt1[j][i].var_h[i][j] != createcomplexf((float)(2*(i*DIM2+j+9)+6),(float)(2*(i*DIM2+j+9)+6)) ) exit(195);
#endif

       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_a[i][j] != 2*(i*DIM2+j+2)-1 ) exit(197);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_b[i][j] != (double)(2*(i*DIM2+j+3))+I*(double)(2*(i*DIM2+j+3)) ) exit(199);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_b[i][j] != createcomplex((double)(2*(i*DIM2+j+3)),(double)(2*(i*DIM2+j+3))) ) exit(199);
#endif
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_c[i][j] != 2*(i*DIM2+j+4)+1 ) exit(201);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_d[i][j] != (float)(2*(i*DIM2+j+5)+2)+I*(float)(2*(i*DIM2+j+5)+2) ) exit(203);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_d[i][j] != createcomplexf((float)(2*(i*DIM2+j+5)+2),(float)(2*(i*DIM2+j+5)+2)) ) exit(203);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+6)+3)+I*(long double)(2*(i*DIM2+j+6)+3) ) exit(205);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_e[i][j] != createcomplexl((long double)(2*(i*DIM2+j+6)+3),(long double)(2*(i*DIM2+j+6)+3)) ) exit(205);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_f[i][j] != (double)(2*(i*DIM2+j+7)+4)+I*(double)(2*(i*DIM2+j+7)+4) ) exit(207);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_f[i][j] != createcomplex((double)(2*(i*DIM2+j+7)+4),(double)(2*(i*DIM2+j+7)+4)) ) exit(207);
#endif
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_g[i][j] != 2*(i*DIM2+j+8)+5 ) exit(209);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+9)+6)+I*(float)(2*(i*DIM2+j+9)+6) ) exit(211);
#else
       if ( (dtr+k*DIM1+l)->vdt2[j][i].vdt1[j][i].var_h[i][j] != createcomplexf((float)(2*(i*DIM2+j+9)+6),(float)(2*(i*DIM2+j+9)+6)) ) exit(211);
#endif
      }
     }
    }
   }

   return 0;
}

DT1 *st1sum(DT1 *dtx, DT1 *dty) {
   DT1 *dtp;
   int i, j, k;

   dtp = malloc(sizeof(DT1));

   for ( i = 0; i < DIM1; i++ ) {
    for ( j = 0; j < DIM2; j++ ) {
     dtp->var_a[i][j] = dtx->var_a[i][j] + dty->var_a[i][j];
     dtp->var_b[i][j] = dtx->var_b[i][j] + dty->var_b[i][j];
     dtp->var_c[i][j] = dtx->var_c[i][j] + dty->var_c[i][j];
     dtp->var_d[i][j] = dtx->var_d[i][j] + dty->var_d[i][j];
     dtp->var_e[i][j] = dtx->var_e[i][j] + dty->var_e[i][j];
     dtp->var_f[i][j] = dtx->var_f[i][j] + dty->var_f[i][j];
     dtp->var_g[i][j] = dtx->var_g[i][j] + dty->var_g[i][j];
     dtp->var_h[i][j] = dtx->var_h[i][j] + dty->var_h[i][j];
    }
   }

   return(dtp);
}

DT2 *st2sum(DT2 *dtx, DT2 *dty) {
   DT2 *dtp;
   DT1 *dtq;
   int i, j, k;

   dtp = malloc(sizeof(DT2));

   for ( i = 0; i < DIM1; i++ ) {
    for ( j = 0; j < DIM2; j++ ) {
     dtp->var_a[i][j] = dtx->var_a[i][j] + dty->var_a[i][j];
     dtp->var_b[i][j] = dtx->var_b[i][j] + dty->var_b[i][j];
     dtp->var_c[i][j] = dtx->var_c[i][j] + dty->var_c[i][j];
     dtp->var_d[i][j] = dtx->var_d[i][j] + dty->var_d[i][j];
     dtp->var_e[i][j] = dtx->var_e[i][j] + dty->var_e[i][j];
     dtp->var_f[i][j] = dtx->var_f[i][j] + dty->var_f[i][j];
     dtp->var_g[i][j] = dtx->var_g[i][j] + dty->var_g[i][j];
     dtp->var_h[i][j] = dtx->var_h[i][j] + dty->var_h[i][j];

     dtq = st1sum(&dtx->vdt1[j][i],&dty->vdt1[j][i]);
     dtp->vdt1[j][i] = *dtq;
    }
   }

   return(dtp);
}

DT3 *st3sum(DT3 *dtx, DT3 *dty) {
   DT3 *dtp;
   DT2 *dtq;
   int i, j, k;

   dtp = malloc(sizeof(DT3)*DIM1*DIM2);

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

      dtq = st2sum(&(dtx+k)->vdt2[j][i],&(dty+k)->vdt2[j][i]);
      (dtp+k)->vdt2[j][i] = *dtq;
     }
    }
   }

   return(dtp);
}

