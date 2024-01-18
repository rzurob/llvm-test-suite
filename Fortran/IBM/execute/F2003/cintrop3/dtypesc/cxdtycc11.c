
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

   DT2 fun1(DT2 *dt);
   DT2 fun2(DT2 dt);
   DT2 fun3(DT2 *dtx, DT2 *dty);
   DT2 *fun4(DT2 *dtx, DT2 *dty);

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

   DT2 dta, dtb, dtc, *dtp;

/* Test 1 */

   dta = dt0;

   dtb = fun1(&dta);

#ifdef CMPLX
   if ( dta.var_a != 3.0f+I*3.0f || dtb.var_a != 3.0f+I*3.0f ) exit(21);
#else
   if ( dta.var_a != createcomplexf(3.0f,3.0f) || dtb.var_a != createcomplexf(3.0f,3.0f) ) exit(21);
#endif
   if ( dta.var_b != 5 || dtb.var_b != 5 ) exit(23);
#ifdef CMPLX
   if ( dta.var_c != 7.0l+I*7.0l || dtb.var_c != 7.0l+I*7.0l ) exit(25);
#else
   if ( dta.var_c != createcomplexl(7.0l,7.0l) || dtb.var_c != createcomplexl(7.0l,7.0l) ) exit(25);
#endif
#ifdef CMPLX
   if ( dta.var_d != 9.0+I*9.0 || dtb.var_d != 9.0+I*9.0 ) exit(27);
#else
   if ( dta.var_d != createcomplex(9.0,9.0) || dtb.var_d != createcomplex(9.0,9.0) ) exit(27);
#endif
#ifdef CMPLX
   if ( dta.var_e != 11.0f+I*11.0f || dtb.var_e != 11.0f+I*11.0f ) exit(29);
#else
   if ( dta.var_e != createcomplexf(11.0f,11.0f) || dtb.var_e != createcomplexf(11.0f,11.0f) ) exit(29);
#endif
   if ( dta.var_f != 13 || dtb.var_f != 13 ) exit(31);
#ifdef CMPLX
   if ( dta.var_g != 15.0+I*15.0 || dtb.var_g != 15.0+I*15.0 ) exit(33);
#else
   if ( dta.var_g != createcomplex(15.0,15.0) || dtb.var_g != createcomplex(15.0,15.0) ) exit(33);
#endif
   if ( dta.var_h != 17 || dtb.var_h != 17 ) exit(35);

   if ( dta.vdt1.var_a != 4 || dtb.vdt1.var_a != 4 ) exit(37);
#ifdef CMPLX
   if ( dta.vdt1.var_b != 6.0+I*6.0 || dtb.vdt1.var_b != 6.0+I*6.0 ) exit(39);
#else
   if ( dta.vdt1.var_b != createcomplex(6.0,6.0) || dtb.vdt1.var_b != createcomplex(6.0,6.0) ) exit(39);
#endif
   if ( dta.vdt1.var_c != 8 || dtb.vdt1.var_c != 8 ) exit(41);
#ifdef CMPLX
   if ( dta.vdt1.var_d != 10.0f+I*10.0f || dtb.vdt1.var_d != 10.0f+I*10.0f ) exit(43);
#else
   if ( dta.vdt1.var_d != createcomplexf(10.0f,10.0f) || dtb.vdt1.var_d != createcomplexf(10.0f,10.0f) ) exit(43);
#endif
#ifdef CMPLX
   if ( dta.vdt1.var_e != 12.0l+I*12.0l || dtb.vdt1.var_e != 12.0l+I*12.0l ) exit(45);
#else
   if ( dta.vdt1.var_e != createcomplexl(12.0l,12.0l) || dtb.vdt1.var_e != createcomplexl(12.0l,12.0l) ) exit(45);
#endif
#ifdef CMPLX
   if ( dta.vdt1.var_f != 14.0+I*14.0 || dtb.vdt1.var_f != 14.0+I*14.0 ) exit(47);
#else
   if ( dta.vdt1.var_f != createcomplex(14.0,14.0) || dtb.vdt1.var_f != createcomplex(14.0,14.0) ) exit(47);
#endif
   if ( dta.vdt1.var_g != 16 || dtb.vdt1.var_g != 16 ) exit(49);
#ifdef CMPLX
   if ( dta.vdt1.var_h != 18.0f+I*18.0f || dtb.vdt1.var_h != 18.0f+I*18.0f ) exit(51);
#else
   if ( dta.vdt1.var_h != createcomplexf(18.0f,18.0f) || dtb.vdt1.var_h != createcomplexf(18.0f,18.0f) ) exit(51);
#endif

/* Test 2 */

   dta = dt0;

   dtb = fun2(dta);

#ifdef CMPLX
   if ( dta.var_a != 2.0f+I*2.0f || dtb.var_a != 3.0f+I*3.0f ) exit(53);
#else
   if ( dta.var_a != createcomplexf(2.0f,2.0f) || dtb.var_a != createcomplexf(3.0f,3.0f) ) exit(53);
#endif
   if ( dta.var_b != 4 || dtb.var_b != 5 ) exit(55);
#ifdef CMPLX
   if ( dta.var_c != 6.0l+I*6.0l || dtb.var_c != 7.0l+I*7.0l ) exit(57);
#else
   if ( dta.var_c != createcomplexl(6.0l,6.0l) || dtb.var_c != createcomplexl(7.0l,7.0l) ) exit(57);
#endif
#ifdef CMPLX
   if ( dta.var_d != 8.0+I*8.0 || dtb.var_d != 9.0+I*9.0 ) exit(59);
#else
   if ( dta.var_d != createcomplex(8.0,8.0) || dtb.var_d != createcomplex(9.0,9.0) ) exit(59);
#endif
#ifdef CMPLX
   if ( dta.var_e != 10.0f+I*10.0f || dtb.var_e != 11.0f+I*11.0f ) exit(61);
#else
   if ( dta.var_e != createcomplexf(10.0f,10.0f) || dtb.var_e != createcomplexf(11.0f,11.0f) ) exit(61);
#endif
   if ( dta.var_f != 12 || dtb.var_f != 13 ) exit(63);
#ifdef CMPLX
   if ( dta.var_g != 14.0+I*14.0 || dtb.var_g != 15.0+I*15.0 ) exit(65);
#else
   if ( dta.var_g != createcomplex(14.0,14.0) || dtb.var_g != createcomplex(15.0,15.0) ) exit(65);
#endif
   if ( dta.var_h != 16 || dtb.var_h != 17 ) exit(67);

   if ( dta.vdt1.var_a != 2 || dtb.vdt1.var_a != 4 ) exit(69);
#ifdef CMPLX
   if ( dta.vdt1.var_b != 4.0+I*4.0 || dtb.vdt1.var_b != 6.0+I*6.0 ) exit(71);
#else
   if ( dta.vdt1.var_b != createcomplex(4.0,4.0) || dtb.vdt1.var_b != createcomplex(6.0,6.0) ) exit(71);
#endif
   if ( dta.vdt1.var_c != 6 || dtb.vdt1.var_c != 8 ) exit(73);
#ifdef CMPLX
   if ( dta.vdt1.var_d != 8.0f+I*8.0f || dtb.vdt1.var_d != 10.0f+I*10.0f ) exit(75);
#else
   if ( dta.vdt1.var_d != createcomplexf(8.0f,8.0f) || dtb.vdt1.var_d != createcomplexf(10.0f,10.0f) ) exit(75);
#endif
#ifdef CMPLX
   if ( dta.vdt1.var_e != 10.0l+I*10.0l || dtb.vdt1.var_e != 12.0l+I*12.0l ) exit(77);
#else
   if ( dta.vdt1.var_e != createcomplexl(10.0l,10.0l) || dtb.vdt1.var_e != createcomplexl(12.0l,12.0l) ) exit(77);
#endif
#ifdef CMPLX
   if ( dta.vdt1.var_f != 12.0+I*12.0 || dtb.vdt1.var_f != 14.0+I*14.0 ) exit(79);
#else
   if ( dta.vdt1.var_f != createcomplex(12.0,12.0) || dtb.vdt1.var_f != createcomplex(14.0,14.0) ) exit(79);
#endif
   if ( dta.vdt1.var_g != 14 || dtb.vdt1.var_g != 16 ) exit(81);
#ifdef CMPLX
   if ( dta.vdt1.var_h != 16.0f+I*16.0f || dtb.vdt1.var_h != 18.0f+I*18.0f ) exit(83);
#else
   if ( dta.vdt1.var_h != createcomplexf(16.0f,16.0f) || dtb.vdt1.var_h != createcomplexf(18.0f,18.0f) ) exit(83);
#endif

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT2));
   dtp = st2sum(&dta,&dta);

   dtc = fun3(dtp,&dtb);

#ifdef CMPLX
   if ( dtb.var_a != 5.0f+I*5.0f || dtc.var_a != 5.0f+I*5.0f ) exit(85);
#else
   if ( dtb.var_a != createcomplexf(5.0f,5.0f) || dtc.var_a != createcomplexf(5.0f,5.0f) ) exit(85);
#endif
   if ( dtb.var_b != 9 || dtc.var_b != 9 ) exit(87);
#ifdef CMPLX
   if ( dtb.var_c != 13.0l+I*13.0l || dtc.var_c != 13.0l+I*13.0l ) exit(89);
#else
   if ( dtb.var_c != createcomplexl(13.0l,13.0l) || dtc.var_c != createcomplexl(13.0l,13.0l) ) exit(89);
#endif
#ifdef CMPLX
   if ( dtb.var_d != 17.0+I*17.0 || dtc.var_d != 17.0+I*17.0 ) exit(91);
#else
   if ( dtb.var_d != createcomplex(17.0,17.0) || dtc.var_d != createcomplex(17.0,17.0) ) exit(91);
#endif
#ifdef CMPLX
   if ( dtb.var_e != 21.0f+I*21.0f || dtc.var_e != 21.0f+I*21.0f ) exit(93);
#else
   if ( dtb.var_e != createcomplexf(21.0f,21.0f) || dtc.var_e != createcomplexf(21.0f,21.0f) ) exit(93);
#endif
   if ( dtb.var_f != 25 || dtc.var_f != 25 ) exit(95);
#ifdef CMPLX
   if ( dtb.var_g != 29.0+I*29.0 || dtc.var_g != 29.0+I*29.0 ) exit(97);
#else
   if ( dtb.var_g != createcomplex(29.0,29.0) || dtc.var_g != createcomplex(29.0,29.0) ) exit(97);
#endif
   if ( dtb.var_h != 33 || dtc.var_h != 33 ) exit(99);

   if ( dtb.vdt1.var_a != 6 || dtc.vdt1.var_a != 6 ) exit(101);
#ifdef CMPLX
   if ( dtb.vdt1.var_b != 10.0+I*10.0 || dtc.vdt1.var_b != 10.0+I*10.0 ) exit(103);
#else
   if ( dtb.vdt1.var_b != createcomplex(10.0,10.0) || dtc.vdt1.var_b != createcomplex(10.0,10.0) ) exit(103);
#endif
   if ( dtb.vdt1.var_c != 14 || dtc.vdt1.var_c != 14 ) exit(105);
#ifdef CMPLX
   if ( dtb.vdt1.var_d != 18.0f+I*18.0f || dtc.vdt1.var_d != 18.0f+I*18.0f ) exit(107);
#else
   if ( dtb.vdt1.var_d != createcomplexf(18.0f,18.0f) || dtc.vdt1.var_d != createcomplexf(18.0f,18.0f) ) exit(107);
#endif
#ifdef CMPLX
   if ( dtb.vdt1.var_e != 22.0l+I*22.0l || dtc.vdt1.var_e != 22.0l+I*22.0l ) exit(109);
#else
   if ( dtb.vdt1.var_e != createcomplexl(22.0l,22.0l) || dtc.vdt1.var_e != createcomplexl(22.0l,22.0l) ) exit(109);
#endif
#ifdef CMPLX
   if ( dtb.vdt1.var_f != 26.0+I*26.0 || dtc.vdt1.var_f != 26.0+I*26.0 ) exit(111);
#else
   if ( dtb.vdt1.var_f != createcomplex(26.0,26.0) || dtc.vdt1.var_f != createcomplex(26.0,26.0) ) exit(111);
#endif
   if ( dtb.vdt1.var_g != 30 || dtc.vdt1.var_g != 30 ) exit(113);
#ifdef CMPLX
   if ( dtb.vdt1.var_h != 34.0f+I*34.0f || dtc.vdt1.var_h != 34.0f+I*34.0f ) exit(115);
#else
   if ( dtb.vdt1.var_h != createcomplexf(34.0f,34.0f) || dtc.vdt1.var_h != createcomplexf(34.0f,34.0f) ) exit(115);
#endif

/* Test 4 */

   dta = dt0;
   dtp = malloc(sizeof(DT2));
   dtp = st2sum(&dta,&dta);

   dtp = fun4(dtp,&dtb);

#ifdef CMPLX
   if ( dtb.var_a != 5.0f+I*5.0f || dtp->var_a != 5.0f+I*5.0f ) exit(117);
#else
   if ( dtb.var_a != createcomplexf(5.0f,5.0f) || dtp->var_a != createcomplexf(5.0f,5.0f) ) exit(117);
#endif
   if ( dtb.var_b != 9 || dtp->var_b != 9 ) exit(119);
#ifdef CMPLX
   if ( dtb.var_c != 13.0l+I*13.0l || dtp->var_c != 13.0l+I*13.0l ) exit(121);
#else
   if ( dtb.var_c != createcomplexl(13.0l,13.0l) || dtp->var_c != createcomplexl(13.0l,13.0l) ) exit(121);
#endif
#ifdef CMPLX
   if ( dtb.var_d != 17.0+I*17.0 || dtp->var_d != 17.0+I*17.0 ) exit(123);
#else
   if ( dtb.var_d != createcomplex(17.0,17.0) || dtp->var_d != createcomplex(17.0,17.0) ) exit(123);
#endif
#ifdef CMPLX
   if ( dtb.var_e != 21.0f+I*21.0f || dtp->var_e != 21.0f+I*21.0f ) exit(125);
#else
   if ( dtb.var_e != createcomplexf(21.0f,21.0f) || dtp->var_e != createcomplexf(21.0f,21.0f) ) exit(125);
#endif
   if ( dtb.var_f != 25 || dtp->var_f != 25 ) exit(127);
#ifdef CMPLX
   if ( dtb.var_g != 29.0+I*29.0 || dtp->var_g != 29.0+I*29.0 ) exit(129);
#else
   if ( dtb.var_g != createcomplex(29.0,29.0) || dtp->var_g != createcomplex(29.0,29.0) ) exit(129);
#endif
   if ( dtb.var_h != 33 || dtp->var_h != 33 ) exit(131);

   if ( dtb.vdt1.var_a != 6 || dtp->vdt1.var_a != 6 ) exit(133);
#ifdef CMPLX
   if ( dtb.vdt1.var_b != 10.0+I*10.0 || dtp->vdt1.var_b != 10.0+I*10.0 ) exit(135);
#else
   if ( dtb.vdt1.var_b != createcomplex(10.0,10.0) || dtp->vdt1.var_b != createcomplex(10.0,10.0) ) exit(135);
#endif
   if ( dtb.vdt1.var_c != 14 || dtp->vdt1.var_c != 14 ) exit(137);
#ifdef CMPLX
   if ( dtb.vdt1.var_d != 18.0f+I*18.0f || dtp->vdt1.var_d != 18.0f+I*18.0f ) exit(139);
#else
   if ( dtb.vdt1.var_d != createcomplexf(18.0f,18.0f) || dtp->vdt1.var_d != createcomplexf(18.0f,18.0f) ) exit(139);
#endif
#ifdef CMPLX
   if ( dtb.vdt1.var_e != 22.0l+I*22.0l || dtp->vdt1.var_e != 22.0l+I*22.0l ) exit(141);
#else
   if ( dtb.vdt1.var_e != createcomplexl(22.0l,22.0l) || dtp->vdt1.var_e != createcomplexl(22.0l,22.0l) ) exit(141);
#endif
#ifdef CMPLX
   if ( dtb.vdt1.var_f != 26.0+I*26.0 || dtp->vdt1.var_f != 26.0+I*26.0 ) exit(143);
#else
   if ( dtb.vdt1.var_f != createcomplex(26.0,26.0) || dtp->vdt1.var_f != createcomplex(26.0,26.0) ) exit(143);
#endif
   if ( dtb.vdt1.var_g != 30 || dtp->vdt1.var_g != 30 ) exit(145);
#ifdef CMPLX
   if ( dtb.vdt1.var_h != 34.0f+I*34.0f || dtp->vdt1.var_h != 34.0f+I*34.0f ) exit(147);
#else
   if ( dtb.vdt1.var_h != createcomplexf(34.0f,34.0f) || dtp->vdt1.var_h != createcomplexf(34.0f,34.0f) ) exit(147);
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
