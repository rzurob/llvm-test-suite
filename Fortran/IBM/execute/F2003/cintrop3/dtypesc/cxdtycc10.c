
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

struct dt1 {
   int_fast16_t var_a;
   double _Complex var_b;
   signed char var_c;
   float _Complex var_d;
   long double _Complex var_e;
   double _Complex var_f;
   intmax_t var_g;
   float _Complex var_h;
};

struct dt2 {
   float _Complex var_a;
   short var_b;
   long double _Complex var_c;
   double _Complex var_d;
   float _Complex var_e;
   char var_f;
   double _Complex var_g;
   int var_h;
   DT1 vdt1;
};

DT1 *st1sum(DT1 *dtx, DT1 *dty);
DT2 *st2sum(DT2 *dtx, DT2 *dty);

int main() {

   void sub1(DT2 *dt);
   void sub2(DT2 dt);
   void sub3(DT2 *dtx, DT2 *dty);

#ifdef CMPLX
   DT2 dt0 = {2.0f+I*2.0f,4,6.0l+I*6.0l,8.0+I*8.0,10.0f+I*10.0f,12,14.0+I*14.0,16,
#else
   DT2 dt0 = {createcomplexf(2.0f,2.0f),4,createcomplexl(6.0l,6.0l),createcomplex(8.0,8.0),createcomplexf(10.0f,10.0f),12,createcomplex(14.0,14.0),16,
#endif
#ifdef CMPLX
             {2,4.0+I*4.0,6,8.0f+I*8.0f,10.0l+I*10.0l,12.0+I*12.0,14,16.0f+I*16.0f}};
#else
             {2,createcomplex(4.0,4.0),6,createcomplexf(8.0f,8.0f),createcomplexl(10.0l,10.0l),createcomplex(12.0,12.0),14,createcomplexf(16.0f,16.0f)}};
#endif

   DT2 dta, dtb, *dtp;

/* Test 1 */

   dta = dt0;

   sub1(&dta);

#ifdef CMPLX
   if ( dta.var_a != 3.0f+I*3.0f || dta.vdt1.var_a != 4 ) exit(21);
#else
   if ( dta.var_a != createcomplexf(3.0f,3.0f) || dta.vdt1.var_a != 4 ) exit(21);
#endif
#ifdef CMPLX
   if ( dta.var_b != 5 || dta.vdt1.var_b != 6.0+I*6.0 ) exit(23);
#else
   if ( dta.var_b != 5 || dta.vdt1.var_b != createcomplex(6.0,6.0) ) exit(23);
#endif
#ifdef CMPLX
   if ( dta.var_c != 7.0l+I*7.0l || dta.vdt1.var_c != 8 ) exit(25);
#else
   if ( dta.var_c != createcomplexl(7.0l,7.0l) || dta.vdt1.var_c != 8 ) exit(25);
#endif
#ifdef CMPLX
   if ( dta.var_d != 9.0+I*9.0 || dta.vdt1.var_d != 10.0f+I*10.0f ) exit(27);
#else
   if ( dta.var_d != createcomplex(9.0,9.0) || dta.vdt1.var_d != createcomplexf(10.0f,10.0f) ) exit(27);
#endif
#ifdef CMPLX
   if ( dta.var_e != 11.0f+I*11.0f || dta.vdt1.var_e != 12.0l+I*12.0l ) exit(29);
#else
   if ( dta.var_e != createcomplexf(11.0f,11.0f) || dta.vdt1.var_e != createcomplexl(12.0l,12.0l) ) exit(29);
#endif
#ifdef CMPLX
   if ( dta.var_f != 13 || dta.vdt1.var_f != 14.0+I*14.0 ) exit(31);
#else
   if ( dta.var_f != 13 || dta.vdt1.var_f != createcomplex(14.0,14.0) ) exit(31);
#endif
#ifdef CMPLX
   if ( dta.var_g != 15.0+I*15.0 || dta.vdt1.var_g != 16 ) exit(33);
#else
   if ( dta.var_g != createcomplex(15.0,15.0) || dta.vdt1.var_g != 16 ) exit(33);
#endif
#ifdef CMPLX
   if ( dta.var_h != 17 || dta.vdt1.var_h != 18.0f+I*18.0f ) exit(35);
#else
   if ( dta.var_h != 17 || dta.vdt1.var_h != createcomplexf(18.0f,18.0f) ) exit(35);
#endif

/* Test 2 */

   dta = dt0;

   sub2(dta);

#ifdef CMPLX
   if ( dta.var_a != 2.0f+I*2.0f || dta.vdt1.var_a != 2 ) exit(37);
#else
   if ( dta.var_a != createcomplexf(2.0f,2.0f) || dta.vdt1.var_a != 2 ) exit(37);
#endif
#ifdef CMPLX
   if ( dta.var_b != 4 || dta.vdt1.var_b != 4.0+I*4.0 ) exit(39);
#else
   if ( dta.var_b != 4 || dta.vdt1.var_b != createcomplex(4.0,4.0) ) exit(39);
#endif
#ifdef CMPLX
   if ( dta.var_c != 6.0l+I*6.0l || dta.vdt1.var_c != 6 ) exit(41);
#else
   if ( dta.var_c != createcomplexl(6.0l,6.0l) || dta.vdt1.var_c != 6 ) exit(41);
#endif
#ifdef CMPLX
   if ( dta.var_d != 8.0+I*8.0 || dta.vdt1.var_d != 8.0f+I*8.0f ) exit(43);
#else
   if ( dta.var_d != createcomplex(8.0,8.0) || dta.vdt1.var_d != createcomplexf(8.0f,8.0f) ) exit(43);
#endif
#ifdef CMPLX
   if ( dta.var_e != 10.0f+I*10.0f || dta.vdt1.var_e != 10.0l+I*10.0l ) exit(45);
#else
   if ( dta.var_e != createcomplexf(10.0f,10.0f) || dta.vdt1.var_e != createcomplexl(10.0l,10.0l) ) exit(45);
#endif
#ifdef CMPLX
   if ( dta.var_f != 12 || dta.vdt1.var_f != 12.0+I*12.0 ) exit(47);
#else
   if ( dta.var_f != 12 || dta.vdt1.var_f != createcomplex(12.0,12.0) ) exit(47);
#endif
#ifdef CMPLX
   if ( dta.var_g != 14.0+I*14.0 || dta.vdt1.var_g != 14 ) exit(49);
#else
   if ( dta.var_g != createcomplex(14.0,14.0) || dta.vdt1.var_g != 14 ) exit(49);
#endif
#ifdef CMPLX
   if ( dta.var_h != 16 || dta.vdt1.var_h != 16.0f+I*16.0f ) exit(51);
#else
   if ( dta.var_h != 16 || dta.vdt1.var_h != createcomplexf(16.0f,16.0f) ) exit(51);
#endif

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT2));
   dtp = st2sum(&dta,&dta);

   sub3(dtp,&dtb);

#ifdef CMPLX
   if ( dtb.var_a != 5.0f+I*5.0f || dtb.vdt1.var_a != 6 ) exit(53);
#else
   if ( dtb.var_a != createcomplexf(5.0f,5.0f) || dtb.vdt1.var_a != 6 ) exit(53);
#endif
#ifdef CMPLX
   if ( dtb.var_b != 9 || dtb.vdt1.var_b != 10.0+I*10.0 ) exit(55);
#else
   if ( dtb.var_b != 9 || dtb.vdt1.var_b != createcomplex(10.0,10.0) ) exit(55);
#endif
#ifdef CMPLX
   if ( dtb.var_c != 13.0l+I*13.0l || dtb.vdt1.var_c != 14 ) exit(57);
#else
   if ( dtb.var_c != createcomplexl(13.0l,13.0l) || dtb.vdt1.var_c != 14 ) exit(57);
#endif
#ifdef CMPLX
   if ( dtb.var_d != 17.0+I*17.0 || dtb.vdt1.var_d != 18.0f+I*18.0f ) exit(59);
#else
   if ( dtb.var_d != createcomplex(17.0,17.0) || dtb.vdt1.var_d != createcomplexf(18.0f,18.0f) ) exit(59);
#endif
#ifdef CMPLX
   if ( dtb.var_e != 21.0f+I*21.0f || dtb.vdt1.var_e != 22.0l+I*22.0l ) exit(61);
#else
   if ( dtb.var_e != createcomplexf(21.0f,21.0f) || dtb.vdt1.var_e != createcomplexl(22.0l,22.0l) ) exit(61);
#endif
#ifdef CMPLX
   if ( dtb.var_f != 25 || dtb.vdt1.var_f != 26.0+I*26.0 ) exit(63);
#else
   if ( dtb.var_f != 25 || dtb.vdt1.var_f != createcomplex(26.0,26.0) ) exit(63);
#endif
#ifdef CMPLX
   if ( dtb.var_g != 29.0+I*29.0 || dtb.vdt1.var_g != 30 ) exit(65);
#else
   if ( dtb.var_g != createcomplex(29.0,29.0) || dtb.vdt1.var_g != 30 ) exit(65);
#endif
#ifdef CMPLX
   if ( dtb.var_h != 33 || dtb.vdt1.var_h != 34.0f+I*34.0f ) exit(67);
#else
   if ( dtb.var_h != 33 || dtb.vdt1.var_h != createcomplexf(34.0f,34.0f) ) exit(67);
#endif

   return 0;
}

DT2 *st2sum(DT2 *dtx, DT2 *dty) {
   DT2 *dtp;
   DT1 *dtq;

   dtp = malloc(sizeof(DT2));

   dtp->var_a = dtx->var_a + dty->var_a;
   dtp->var_b = dtx->var_b + dty->var_b;
   dtp->var_c = dtx->var_c + dty->var_c;
   dtp->var_d = dtx->var_d + dty->var_d;
   dtp->var_e = dtx->var_e + dty->var_e;
   dtp->var_f = dtx->var_f + dty->var_f;
   dtp->var_g = dtx->var_g + dty->var_g;
   dtp->var_h = dtx->var_h + dty->var_h;
   dtq = st1sum(&dtx->vdt1,&dty->vdt1);
   dtp->vdt1 = *dtq;

   return(dtp);
}

DT1 *st1sum(DT1 *dtx, DT1 *dty) {
   DT1 *dtp;

   dtp = malloc(sizeof(DT1));

   dtp->var_a = dtx->var_a + dty->var_a;
   dtp->var_b = dtx->var_b + dty->var_b;
   dtp->var_c = dtx->var_c + dty->var_c;
   dtp->var_d = dtx->var_d + dty->var_d;
   dtp->var_e = dtx->var_e + dty->var_e;
   dtp->var_f = dtx->var_f + dty->var_f;
   dtp->var_g = dtx->var_g + dty->var_g;
   dtp->var_h = dtx->var_h + dty->var_h;

   return(dtp);
}
