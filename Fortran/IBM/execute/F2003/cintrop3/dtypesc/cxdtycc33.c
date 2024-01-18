
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

#ifdef CMPLX
#define ARRDT1 {1,2,3,4,5,6},{2.0+I*2.0,3.0+I*3.0,4.0+I*4.0,5.0+I*5.0,6.0+I*6.0,7.0+I*7.0},{3,4,5,6,7,8},{4.0f+I*4.0f,5.0f+I*5.0f,6.0f+I*6.0f,7.0f+I*7.0f,8.0f+I*8.0f,9.0f+I*9.0f},{5.0l+I*5.0l,6.0l+I*6.0l,7.0l+I*7.0l,8.0l+I*8.0l,9.0l+I*9.0l,10.0l+I*10.0l},{6.0+I*6.0,7.0+I*7.0,8.0+I*8.0,9.0+I*9.0,10.0+I*10.0,11.0+I*11.0},{7,8,9,10,11,12},{8.0f+I*8.0f,9.0f+I*9.0f,10.0f+I*10.0f,11.0f+I*11.0f,12.0f+I*12.0f,13.0f+I*13.0f}
#else
#define ARRDT1 {1,2,3,4,5,6},{createcomplex(2.0,2.0),createcomplex(3.0,3.0),createcomplex(4.0,4.0),createcomplex(5.0,5.0),createcomplex(6.0,6.0),createcomplex(7.0,7.0)},{3,4,5,6,7,8},{createcomplexf(4.0f,4.0f),createcomplexf(5.0f,5.0f),createcomplexf(6.0f,6.0f),createcomplexf(7.0f,7.0f),createcomplexf(8.0f,8.0f),createcomplexf(9.0f,9.0f)},{createcomplexl(5.0l,5.0l),createcomplexl(6.0l,6.0l),createcomplexl(7.0l,7.0l),createcomplexl(8.0l,8.0l),createcomplexl(9.0l,9.0l),createcomplexl(10.0l,10.0l)},{createcomplex(6.0,6.0),createcomplex(7.0,7.0),createcomplex(8.0,8.0),createcomplex(9.0,9.0),createcomplex(10.0,10.0),createcomplex(11.0,11.0)},{7,8,9,10,11,12},{createcomplexf(8.0f,8.0f),createcomplexf(9.0f,9.0f),createcomplexf(10.0f,10.0f),createcomplexf(11.0f,11.0f),createcomplexf(12.0f,12.0f),createcomplexf(13.0f,13.0f)}
#endif
#ifdef CMPLX
#define ARR2 {1.0f+I*1.0f,2.0f+I*2.0f,3.0f+I*3.0f,4.0f+I*4.0f,5.0f+I*5.0f,6.0f+I*6.0f},{2,3,4,5,6,7},{3.0l+I*3.0l,4.0l+I*4.0l,5.0l+I*5.0l,6.0l+I*6.0l,7.0l+I*7.0l,8.0l+I*8.0l},{4.0+I*4.0,5.0+I*5.0,6.0+I*6.0,7.0+I*7.0,8.0+I*8.0,9.0+I*9.0},{5.0f+I*5.0f,6.0f+I*6.0f,7.0f+I*7.0f,8.0f+I*8.0f,9.0f+I*9.0f,10.0f+I*10.0f},{6,7,8,9,10,11},{7.0+I*7.0,8.0+I*8.0,9.0+I*9.0,10.0+I*10.0,11.0+I*11.0,12.0+I*12.0},{8,9,10,11,12,13}
#else
#define ARR2 {createcomplexf(1.0f,1.0f),createcomplexf(2.0f,2.0f),createcomplexf(3.0f,3.0f),createcomplexf(4.0f,4.0f),createcomplexf(5.0f,5.0f),createcomplexf(6.0f,6.0f)},{2,3,4,5,6,7},{createcomplexl(3.0l,3.0l),createcomplexl(4.0l,4.0l),createcomplexl(5.0l,5.0l),createcomplexl(6.0l,6.0l),createcomplexl(7.0l,7.0l),createcomplexl(8.0l,8.0l)},{createcomplex(4.0,4.0),createcomplex(5.0,5.0),createcomplex(6.0,6.0),createcomplex(7.0,7.0),createcomplex(8.0,8.0),createcomplex(9.0,9.0)},{createcomplexf(5.0f,5.0f),createcomplexf(6.0f,6.0f),createcomplexf(7.0f,7.0f),createcomplexf(8.0f,8.0f),createcomplexf(9.0f,9.0f),createcomplexf(10.0f,10.0f)},{6,7,8,9,10,11},{createcomplex(7.0,7.0),createcomplex(8.0,8.0),createcomplex(9.0,9.0),createcomplex(10.0,10.0),createcomplex(11.0,11.0),createcomplex(12.0,12.0)},{8,9,10,11,12,13}
#endif

#define ARRDT2 ARR2,{{{ARRDT1},{ARRDT1}},{{ARRDT1},{ARRDT1}},{{ARRDT1},{ARRDT1}}}

DT1 *st1sum(DT1 *, DT1 *);
DT2 *st2sum(DT2 *, DT2 *);

int main() {

   DT2 *fun1(DT2 *);
   DT2 *fun2(DT2 *, DT2 *);

   DT2 dt0 = { ARRDT2 };

   DT2 dta[DIM2][DIM1], dtb[DIM2][DIM1];
   DT2 *dtp, *dtr;
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

       if ( dta[k][l].vdt1[j][i].var_a[i][j] != i*DIM2+j+2 ) exit(53);
#ifdef CMPLX
       if ( dta[k][l].vdt1[j][i].var_b[i][j] != (double)(i*DIM2+j+3)+I*(double)(i*DIM2+j+3) ) exit(55);
#else
       if ( dta[k][l].vdt1[j][i].var_b[i][j] != createcomplex((double)(i*DIM2+j+3),(double)(i*DIM2+j+3)) ) exit(55);
#endif
       if ( dta[k][l].vdt1[j][i].var_c[i][j] != i*DIM2+j+4 ) exit(57);
#ifdef CMPLX
       if ( dta[k][l].vdt1[j][i].var_d[i][j] != (float)(i*DIM2+j+5)+I*(float)(i*DIM2+j+5) ) exit(59);
#else
       if ( dta[k][l].vdt1[j][i].var_d[i][j] != createcomplexf((float)(i*DIM2+j+5),(float)(i*DIM2+j+5)) ) exit(59);
#endif
#ifdef CMPLX
       if ( dta[k][l].vdt1[j][i].var_e[i][j] != (long double)(i*DIM2+j+6)+I*(long double)(i*DIM2+j+6) ) exit(61);
#else
       if ( dta[k][l].vdt1[j][i].var_e[i][j] != createcomplexl((long double)(i*DIM2+j+6),(long double)(i*DIM2+j+6)) ) exit(61);
#endif
#ifdef CMPLX
       if ( dta[k][l].vdt1[j][i].var_f[i][j] != (double)(i*DIM2+j+7)+I*(double)(i*DIM2+j+7) ) exit(63);
#else
       if ( dta[k][l].vdt1[j][i].var_f[i][j] != createcomplex((double)(i*DIM2+j+7),(double)(i*DIM2+j+7)) ) exit(63);
#endif
       if ( dta[k][l].vdt1[j][i].var_g[i][j] != i*DIM2+j+8 ) exit(65);
#ifdef CMPLX
       if ( dta[k][l].vdt1[j][i].var_h[i][j] != (float)(i*DIM2+j+9)+I*(float)(i*DIM2+j+9) ) exit(67);
#else
       if ( dta[k][l].vdt1[j][i].var_h[i][j] != createcomplexf((float)(i*DIM2+j+9),(float)(i*DIM2+j+9)) ) exit(67);
#endif

       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_a[i][j] != i*DIM2+j+2 ) exit(69);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_b[i][j] != (double)(i*DIM2+j+3)+I*(double)(i*DIM2+j+3) ) exit(71);
#else
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_b[i][j] != createcomplex((double)(i*DIM2+j+3),(double)(i*DIM2+j+3)) ) exit(71);
#endif
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_c[i][j] != i*DIM2+j+4 ) exit(73);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_d[i][j] != (float)(i*DIM2+j+5)+I*(float)(i*DIM2+j+5) ) exit(75);
#else
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_d[i][j] != createcomplexf((float)(i*DIM2+j+5),(float)(i*DIM2+j+5)) ) exit(75);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_e[i][j] != (long double)(i*DIM2+j+6)+I*(long double)(i*DIM2+j+6) ) exit(77);
#else
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_e[i][j] != createcomplexl((long double)(i*DIM2+j+6),(long double)(i*DIM2+j+6)) ) exit(77);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_f[i][j] != (double)(i*DIM2+j+7)+I*(double)(i*DIM2+j+7) ) exit(79);
#else
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_f[i][j] != createcomplex((double)(i*DIM2+j+7),(double)(i*DIM2+j+7)) ) exit(79);
#endif
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_g[i][j] != i*DIM2+j+8 ) exit(81);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_h[i][j] != (float)(i*DIM2+j+9)+I*(float)(i*DIM2+j+9) ) exit(83);
#else
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_h[i][j] != createcomplexf((float)(i*DIM2+j+9),(float)(i*DIM2+j+9)) ) exit(83);
#endif
      }
     }
    }
   }

/* Test 2 */

   for ( i = 0; i < DIM2; i++ ) {
     for ( j = 0; j < DIM1; j++ ) {
       dta[i][j] = dt0;
     }
   }

   dtp = malloc(sizeof(DT2)*DIM1*DIM2);
   dtp = st2sum(&dta[0][0],&dta[0][0]);

   dtr = fun2(dtp,&dtb[0][0]);

   for ( k = 0; k < DIM2; k++ ) {
    for ( l = 0; l < DIM1; l++ ) {
     for ( i = 0; i < DIM1; i++ ) {
      for ( j = 0; j < DIM2; j++ ) {
#ifdef CMPLX
       if ( dtb[k][l].var_a[i][j] != (float)(2*(i*DIM2+j+2)-1)+I*(float)(2*(i*DIM2+j+2)-1) ) exit(85);
#else
       if ( dtb[k][l].var_a[i][j] != createcomplexf((float)(2*(i*DIM2+j+2)-1),(float)(2*(i*DIM2+j+2)-1)) ) exit(85);
#endif
       if ( dtb[k][l].var_b[i][j] != 2*(i*DIM2+j+3) ) exit(87);
#ifdef CMPLX
       if ( dtb[k][l].var_c[i][j] != (long double)(2*(i*DIM2+j+4)+1)+I*(long double)(2*(i*DIM2+j+4)+1) ) exit(89);
#else
       if ( dtb[k][l].var_c[i][j] != createcomplexl((long double)(2*(i*DIM2+j+4)+1),(long double)(2*(i*DIM2+j+4)+1)) ) exit(89);
#endif
#ifdef CMPLX
       if ( dtb[k][l].var_d[i][j] != (double)(2*(i*DIM2+j+5)+2)+I*(double)(2*(i*DIM2+j+5)+2) ) exit(91);
#else
       if ( dtb[k][l].var_d[i][j] != createcomplex((double)(2*(i*DIM2+j+5)+2),(double)(2*(i*DIM2+j+5)+2)) ) exit(91);
#endif
#ifdef CMPLX
       if ( dtb[k][l].var_e[i][j] != (float)(2*(i*DIM2+j+6)+3)+I*(float)(2*(i*DIM2+j+6)+3) ) exit(93);
#else
       if ( dtb[k][l].var_e[i][j] != createcomplexf((float)(2*(i*DIM2+j+6)+3),(float)(2*(i*DIM2+j+6)+3)) ) exit(93);
#endif
       if ( dtb[k][l].var_f[i][j] != 2*(i*DIM2+j+7)+4 ) exit(95);
#ifdef CMPLX
       if ( dtb[k][l].var_g[i][j] != (double)(2*(i*DIM2+j+8)+5)+I*(double)(2*(i*DIM2+j+8)+5) ) exit(97);
#else
       if ( dtb[k][l].var_g[i][j] != createcomplex((double)(2*(i*DIM2+j+8)+5),(double)(2*(i*DIM2+j+8)+5)) ) exit(97);
#endif
       if ( dtb[k][l].var_h[i][j] != 2*(i*DIM2+j+9)+6 ) exit(99);

#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_a[i][j] != (float)(2*(i*DIM2+j+2)-1)+I*(float)(2*(i*DIM2+j+2)-1) ) exit(101);
#else
       if ( (dtr+k*DIM1+l)->var_a[i][j] != createcomplexf((float)(2*(i*DIM2+j+2)-1),(float)(2*(i*DIM2+j+2)-1)) ) exit(101);
#endif
       if ( (dtr+k*DIM1+l)->var_b[i][j] != 2*(i*DIM2+j+3) ) exit(103);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_c[i][j] != (long double)(2*(i*DIM2+j+4)+1)+I*(long double)(2*(i*DIM2+j+4)+1) ) exit(105);
#else
       if ( (dtr+k*DIM1+l)->var_c[i][j] != createcomplexl((long double)(2*(i*DIM2+j+4)+1),(long double)(2*(i*DIM2+j+4)+1)) ) exit(105);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_d[i][j] != (double)(2*(i*DIM2+j+5)+2)+I*(double)(2*(i*DIM2+j+5)+2) ) exit(107);
#else
       if ( (dtr+k*DIM1+l)->var_d[i][j] != createcomplex((double)(2*(i*DIM2+j+5)+2),(double)(2*(i*DIM2+j+5)+2)) ) exit(107);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_e[i][j] != (float)(2*(i*DIM2+j+6)+3)+I*(float)(2*(i*DIM2+j+6)+3) ) exit(109);
#else
       if ( (dtr+k*DIM1+l)->var_e[i][j] != createcomplexf((float)(2*(i*DIM2+j+6)+3),(float)(2*(i*DIM2+j+6)+3)) ) exit(109);
#endif
       if ( (dtr+k*DIM1+l)->var_f[i][j] != 2*(i*DIM2+j+7)+4 ) exit(111);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->var_g[i][j] != (double)(2*(i*DIM2+j+8)+5)+I*(double)(2*(i*DIM2+j+8)+5) ) exit(113);
#else
       if ( (dtr+k*DIM1+l)->var_g[i][j] != createcomplex((double)(2*(i*DIM2+j+8)+5),(double)(2*(i*DIM2+j+8)+5)) ) exit(113);
#endif
       if ( (dtr+k*DIM1+l)->var_h[i][j] != 2*(i*DIM2+j+9)+6 ) exit(115);

       if ( dtb[k][l].vdt1[j][i].var_a[i][j] != 2*(i*DIM2+j+2)-1 ) exit(117);
#ifdef CMPLX
       if ( dtb[k][l].vdt1[j][i].var_b[i][j] != (double)(2*(i*DIM2+j+3))+I*(double)(2*(i*DIM2+j+3)) ) exit(119);
#else
       if ( dtb[k][l].vdt1[j][i].var_b[i][j] != createcomplex((double)(2*(i*DIM2+j+3)),(double)(2*(i*DIM2+j+3))) ) exit(119);
#endif
       if ( dtb[k][l].vdt1[j][i].var_c[i][j] != 2*(i*DIM2+j+4)+1 ) exit(121);
#ifdef CMPLX
       if ( dtb[k][l].vdt1[j][i].var_d[i][j] != (float)(2*(i*DIM2+j+5)+2)+I*(float)(2*(i*DIM2+j+5)+2) ) exit(123);
#else
       if ( dtb[k][l].vdt1[j][i].var_d[i][j] != createcomplexf((float)(2*(i*DIM2+j+5)+2),(float)(2*(i*DIM2+j+5)+2)) ) exit(123);
#endif
#ifdef CMPLX
       if ( dtb[k][l].vdt1[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+6)+3)+I*(long double)(2*(i*DIM2+j+6)+3) ) exit(125);
#else
       if ( dtb[k][l].vdt1[j][i].var_e[i][j] != createcomplexl((long double)(2*(i*DIM2+j+6)+3),(long double)(2*(i*DIM2+j+6)+3)) ) exit(125);
#endif
#ifdef CMPLX
       if ( dtb[k][l].vdt1[j][i].var_f[i][j] != (double)(2*(i*DIM2+j+7)+4)+I*(double)(2*(i*DIM2+j+7)+4) ) exit(127);
#else
       if ( dtb[k][l].vdt1[j][i].var_f[i][j] != createcomplex((double)(2*(i*DIM2+j+7)+4),(double)(2*(i*DIM2+j+7)+4)) ) exit(127);
#endif
       if ( dtb[k][l].vdt1[j][i].var_g[i][j] != 2*(i*DIM2+j+8)+5 ) exit(129);
#ifdef CMPLX
       if ( dtb[k][l].vdt1[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+9)+6)+I*(float)(2*(i*DIM2+j+9)+6) ) exit(131);
#else
       if ( dtb[k][l].vdt1[j][i].var_h[i][j] != createcomplexf((float)(2*(i*DIM2+j+9)+6),(float)(2*(i*DIM2+j+9)+6)) ) exit(131);
#endif

       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_a[i][j] != 2*(i*DIM2+j+2)-1 ) exit(133);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_b[i][j] != (double)(2*(i*DIM2+j+3))+I*(double)(2*(i*DIM2+j+3)) ) exit(135);
#else
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_b[i][j] != createcomplex((double)(2*(i*DIM2+j+3)),(double)(2*(i*DIM2+j+3))) ) exit(135);
#endif
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_c[i][j] != 2*(i*DIM2+j+4)+1 ) exit(137);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_d[i][j] != (float)(2*(i*DIM2+j+5)+2)+I*(float)(2*(i*DIM2+j+5)+2) ) exit(139);
#else
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_d[i][j] != createcomplexf((float)(2*(i*DIM2+j+5)+2),(float)(2*(i*DIM2+j+5)+2)) ) exit(139);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_e[i][j] != (long double)(2*(i*DIM2+j+6)+3)+I*(long double)(2*(i*DIM2+j+6)+3) ) exit(141);
#else
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_e[i][j] != createcomplexl((long double)(2*(i*DIM2+j+6)+3),(long double)(2*(i*DIM2+j+6)+3)) ) exit(141);
#endif
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_f[i][j] != (double)(2*(i*DIM2+j+7)+4)+I*(double)(2*(i*DIM2+j+7)+4) ) exit(143);
#else
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_f[i][j] != createcomplex((double)(2*(i*DIM2+j+7)+4),(double)(2*(i*DIM2+j+7)+4)) ) exit(143);
#endif
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_g[i][j] != 2*(i*DIM2+j+8)+5 ) exit(145);
#ifdef CMPLX
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_h[i][j] != (float)(2*(i*DIM2+j+9)+6)+I*(float)(2*(i*DIM2+j+9)+6) ) exit(147);
#else
       if ( (dtr+k*DIM1+l)->vdt1[j][i].var_h[i][j] != createcomplexf((float)(2*(i*DIM2+j+9)+6),(float)(2*(i*DIM2+j+9)+6)) ) exit(147);
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

   dtp = malloc(sizeof(DT2)*DIM1*DIM2);

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

      dtq = st1sum(&(dtx+k)->vdt1[j][i],&(dty+k)->vdt1[j][i]);
      (dtp+k)->vdt1[j][i] = *dtq;
     }
    }
   }

   return(dtp);
}

