
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
   DT1 vdt1;
   double _Complex var_a;
   int var_b;
   double _Complex var_c;
   char var_d;
   long double _Complex var_e;
   float _Complex var_f;
   int32_t var_g;
   float _Complex var_h;
};

struct dt3 {
   float _Complex var_a;
   short var_b;
   long double _Complex var_c;
   double _Complex var_d;
   float _Complex var_e;
   char var_f;
   double _Complex var_g;
   int var_h;
   DT2 vdt2;
};

DT1 *st1sum(DT1 *dtx, DT1 *dty);
DT2 *st2sum(DT2 *dtx, DT2 *dty);
DT3 *st3sum(DT3 *dtx, DT3 *dty);

int main() {

   void sub1(DT3 *dt);
   void sub2(DT3 dt);
   void sub3(DT3 *dtx, DT3 *dty);

#ifdef CMPLX
   DT3 dt0 = {2.0f+I*2.0f,4,6.0l+I*6.0l,8.0+I*8.0,10.0f+I*10.0f,12,14.0+I*14.0,16,
#else
   DT3 dt0 = {createcomplexf(2.0f,2.0f),4,createcomplexl(6.0l,6.0l),createcomplex(8.0,8.0),createcomplexf(10.0f,10.0f),12,createcomplex(14.0,14.0),16,
#endif
#ifdef CMPLX
             {{2,4.0+I*4.0,6,8.0f+I*8.0f,10.0l+I*10.0l,12.0+I*12.0,14,16.0f+I*16.0f},
#else
             {{2,createcomplex(4.0,4.0),6,createcomplexf(8.0f,8.0f),createcomplexl(10.0l,10.0l),createcomplex(12.0,12.0),14,createcomplexf(16.0f,16.0f)},
#endif
#ifdef CMPLX
             2.0+I*2.0,4,6.0+I*6.0,8,10.0l+I*10.0l,12.0f+I*12.0f,14,16.0f+I*16.0f}};
#else
             createcomplex(2.0,2.0),4,createcomplex(6.0,6.0),8,createcomplexl(10.0l,10.0l),createcomplexf(12.0f,12.0f),14,createcomplexf(16.0f,16.0f)}};
#endif

   DT3 dta, dtb, *dtp;

/* Test 1 */

   dta = dt0;

   sub1(&dta);

#ifdef CMPLX
   if ( dta.var_a != 3.0f+I*3.0f || dta.vdt2.var_a != 4.0+I*4.0 ||
#else
   if ( dta.var_a != createcomplexf(3.0f,3.0f) || dta.vdt2.var_a != createcomplex(4.0,4.0) ||
#endif
                          dta.vdt2.vdt1.var_a != 5 ) exit(21);

   if ( dta.var_b != 5 || dta.vdt2.var_b != 6 ||
#ifdef CMPLX
                          dta.vdt2.vdt1.var_b != 7.0+I*7.0 ) exit(23);
#else
                          dta.vdt2.vdt1.var_b != createcomplex(7.0,7.0) ) exit(23);
#endif

#ifdef CMPLX
   if ( dta.var_c != 7.0l+I*7.0l || dta.vdt2.var_c != 8.0+I*8.0 ||
#else
   if ( dta.var_c != createcomplexl(7.0l,7.0l) || dta.vdt2.var_c != createcomplex(8.0,8.0) ||
#endif
                          dta.vdt2.vdt1.var_c != 9 ) exit(25);

#ifdef CMPLX
   if ( dta.var_d != 9.0+I*9.0 || dta.vdt2.var_d != 10 ||
#else
   if ( dta.var_d != createcomplex(9.0,9.0) || dta.vdt2.var_d != 10 ||
#endif
#ifdef CMPLX
                          dta.vdt2.vdt1.var_d != 11.0f+I*11.0f ) exit(27);
#else
                          dta.vdt2.vdt1.var_d != createcomplexf(11.0f,11.0f) ) exit(27);
#endif

#ifdef CMPLX
   if ( dta.var_e != 11.0f+I*11.0f || dta.vdt2.var_e != 12.0l+I*12.0l ||
#else
   if ( dta.var_e != createcomplexf(11.0f,11.0f) || dta.vdt2.var_e != createcomplexl(12.0l,12.0l) ||
#endif
#ifdef CMPLX
                           dta.vdt2.vdt1.var_e != 13.0l+I*13.0l ) exit(29);
#else
                           dta.vdt2.vdt1.var_e != createcomplexl(13.0l,13.0l) ) exit(29);
#endif

#ifdef CMPLX
   if ( dta.var_f != 13 || dta.vdt2.var_f != 14.0f+I*14.0f ||
#else
   if ( dta.var_f != 13 || dta.vdt2.var_f != createcomplexf(14.0f,14.0f) ||
#endif
#ifdef CMPLX
                           dta.vdt2.vdt1.var_f != 15.0+I*15.0 ) exit(31);
#else
                           dta.vdt2.vdt1.var_f != createcomplex(15.0,15.0) ) exit(31);
#endif

#ifdef CMPLX
   if ( dta.var_g != 15.0+I*15.0 || dta.vdt2.var_g != 16 ||
#else
   if ( dta.var_g != createcomplex(15.0,15.0) || dta.vdt2.var_g != 16 ||
#endif
                           dta.vdt2.vdt1.var_g != 17 ) exit(33);

#ifdef CMPLX
   if ( dta.var_h != 17 || dta.vdt2.var_h != 18.0f+I*18.0f ||
#else
   if ( dta.var_h != 17 || dta.vdt2.var_h != createcomplexf(18.0f,18.0f) ||
#endif
#ifdef CMPLX
                           dta.vdt2.vdt1.var_h != 19.0f+I*19.0f ) exit(35);
#else
                           dta.vdt2.vdt1.var_h != createcomplexf(19.0f,19.0f) ) exit(35);
#endif

/* Test 2 */

   dta = dt0;

   sub2(dta);

#ifdef CMPLX
   if ( dta.var_a != 2.0f+I*2.0f || dta.vdt2.var_a != 2.0+I*2.0 ||
#else
   if ( dta.var_a != createcomplexf(2.0f,2.0f) || dta.vdt2.var_a != createcomplex(2.0,2.0) ||
#endif
                          dta.vdt2.vdt1.var_a != 2 ) exit(37);

   if ( dta.var_b != 4 || dta.vdt2.var_b != 4 ||
#ifdef CMPLX
                          dta.vdt2.vdt1.var_b != 4.0+I*4.0 ) exit(39);
#else
                          dta.vdt2.vdt1.var_b != createcomplex(4.0,4.0) ) exit(39);
#endif

#ifdef CMPLX
   if ( dta.var_c != 6.0l+I*6.0l || dta.vdt2.var_c != 6.0+I*6.0 ||
#else
   if ( dta.var_c != createcomplexl(6.0l,6.0l) || dta.vdt2.var_c != createcomplex(6.0,6.0) ||
#endif
                          dta.vdt2.vdt1.var_c != 6 ) exit(41);

#ifdef CMPLX
   if ( dta.var_d != 8.0+I*8.0 || dta.vdt2.var_d != 8 ||
#else
   if ( dta.var_d != createcomplex(8.0,8.0) || dta.vdt2.var_d != 8 ||
#endif
#ifdef CMPLX
                          dta.vdt2.vdt1.var_d != 8.0f+I*8.0f ) exit(43);
#else
                          dta.vdt2.vdt1.var_d != createcomplexf(8.0f,8.0f) ) exit(43);
#endif

#ifdef CMPLX
   if ( dta.var_e != 10.0f+I*10.0f || dta.vdt2.var_e != 10.0l+I*10.0l ||
#else
   if ( dta.var_e != createcomplexf(10.0f,10.0f) || dta.vdt2.var_e != createcomplexl(10.0l,10.0l) ||
#endif
#ifdef CMPLX
                           dta.vdt2.vdt1.var_e != 10.0l+I*10.0l ) exit(45);
#else
                           dta.vdt2.vdt1.var_e != createcomplexl(10.0l,10.0l) ) exit(45);
#endif

#ifdef CMPLX
   if ( dta.var_f != 12 || dta.vdt2.var_f != 12.0f+I*12.0f ||
#else
   if ( dta.var_f != 12 || dta.vdt2.var_f != createcomplexf(12.0f,12.0f) ||
#endif
#ifdef CMPLX
                           dta.vdt2.vdt1.var_f != 12.0+I*12.0 ) exit(47);
#else
                           dta.vdt2.vdt1.var_f != createcomplex(12.0,12.0) ) exit(47);
#endif

#ifdef CMPLX
   if ( dta.var_g != 14.0+I*14.0 || dta.vdt2.var_g != 14 ||
#else
   if ( dta.var_g != createcomplex(14.0,14.0) || dta.vdt2.var_g != 14 ||
#endif
                           dta.vdt2.vdt1.var_g != 14 ) exit(49);

#ifdef CMPLX
   if ( dta.var_h != 16 || dta.vdt2.var_h != 16.0f+I*16.0f ||
#else
   if ( dta.var_h != 16 || dta.vdt2.var_h != createcomplexf(16.0f,16.0f) ||
#endif
#ifdef CMPLX
                           dta.vdt2.vdt1.var_h != 16.0f+I*16.0f ) exit(51);
#else
                           dta.vdt2.vdt1.var_h != createcomplexf(16.0f,16.0f) ) exit(51);
#endif

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT3));
   dtp = st3sum(&dta,&dta);

   sub3(dtp,&dtb);

#ifdef CMPLX
   if ( dtb.var_a != 5.0f+I*5.0f || dtb.vdt2.var_a != 6.0+I*6.0 ||
#else
   if ( dtb.var_a != createcomplexf(5.0f,5.0f) || dtb.vdt2.var_a != createcomplex(6.0,6.0) ||
#endif
                          dtb.vdt2.vdt1.var_a != 7 ) exit(53);

   if ( dtb.var_b != 9 || dtb.vdt2.var_b != 10 ||
#ifdef CMPLX
                          dtb.vdt2.vdt1.var_b != 11.0+I*11.0 ) exit(55);
#else
                          dtb.vdt2.vdt1.var_b != createcomplex(11.0,11.0) ) exit(55);
#endif

#ifdef CMPLX
   if ( dtb.var_c != 13.0l+I*13.0l || dtb.vdt2.var_c != 14.0+I*14.0 ||
#else
   if ( dtb.var_c != createcomplexl(13.0l,13.0l) || dtb.vdt2.var_c != createcomplex(14.0,14.0) ||
#endif
                           dtb.vdt2.vdt1.var_c != 15 ) exit(57);

#ifdef CMPLX
   if ( dtb.var_d != 17.0+I*17.0 || dtb.vdt2.var_d != 18 ||
#else
   if ( dtb.var_d != createcomplex(17.0,17.0) || dtb.vdt2.var_d != 18 ||
#endif
#ifdef CMPLX
                           dtb.vdt2.vdt1.var_d != 19.0f+I*19.0f ) exit(59);
#else
                           dtb.vdt2.vdt1.var_d != createcomplexf(19.0f,19.0f) ) exit(59);
#endif

#ifdef CMPLX
   if ( dtb.var_e != 21.0f+I*21.0f || dtb.vdt2.var_e != 22.0l+I*22.0l ||
#else
   if ( dtb.var_e != createcomplexf(21.0f,21.0f) || dtb.vdt2.var_e != createcomplexl(22.0l,22.0l) ||
#endif
#ifdef CMPLX
                           dtb.vdt2.vdt1.var_e != 23.0l+I*23.0l ) exit(61);
#else
                           dtb.vdt2.vdt1.var_e != createcomplexl(23.0l,23.0l) ) exit(61);
#endif

#ifdef CMPLX
   if ( dtb.var_f != 25 || dtb.vdt2.var_f != 26.0f+I*26.0f ||
#else
   if ( dtb.var_f != 25 || dtb.vdt2.var_f != createcomplexf(26.0f,26.0f) ||
#endif
#ifdef CMPLX
                           dtb.vdt2.vdt1.var_f != 27.0+I*27.0 ) exit(63);
#else
                           dtb.vdt2.vdt1.var_f != createcomplex(27.0,27.0) ) exit(63);
#endif

#ifdef CMPLX
   if ( dtb.var_g != 29.0+I*29.0 || dtb.vdt2.var_g != 30 ||
#else
   if ( dtb.var_g != createcomplex(29.0,29.0) || dtb.vdt2.var_g != 30 ||
#endif
                           dtb.vdt2.vdt1.var_g != 31 ) exit(65);

#ifdef CMPLX
   if ( dtb.var_h != 33 || dtb.vdt2.var_h != 34.0f+I*34.0f ||
#else
   if ( dtb.var_h != 33 || dtb.vdt2.var_h != createcomplexf(34.0f,34.0f) ||
#endif
#ifdef CMPLX
                           dtb.vdt2.vdt1.var_h != 35.0f+I*35.0f ) exit(67);
#else
                           dtb.vdt2.vdt1.var_h != createcomplexf(35.0f,35.0f) ) exit(67);
#endif

   return 0;
}

DT3 *st3sum(DT3 *dtx, DT3 *dty) {
   DT3 *dtp;
   DT2 *dtq;

   dtp = malloc(sizeof(DT3));

   dtp->var_a = dtx->var_a + dty->var_a;
   dtp->var_b = dtx->var_b + dty->var_b;
   dtp->var_c = dtx->var_c + dty->var_c;
   dtp->var_d = dtx->var_d + dty->var_d;
   dtp->var_e = dtx->var_e + dty->var_e;
   dtp->var_f = dtx->var_f + dty->var_f;
   dtp->var_g = dtx->var_g + dty->var_g;
   dtp->var_h = dtx->var_h + dty->var_h;
   dtq = st2sum(&dtx->vdt2,&dty->vdt2);
   dtp->vdt2 = *dtq;

   return(dtp);
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
