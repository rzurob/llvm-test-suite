
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
   double _Complex var_a;
   int var_b;
   double _Complex var_c;
   char var_d;
   long double _Complex var_e;
   float _Complex var_f;
   int32_t var_g;
   float _Complex var_h;
   DT1 vdt1;
};

struct dt3 {
   DT2 vdt2;
   float _Complex var_a;
   short var_b;
   long double _Complex var_c;
   double _Complex var_d;
   float _Complex var_e;
   char var_f;
   double _Complex var_g;
   int var_h;
};

DT1 *st1sum(DT1 *dtx, DT1 *dty);
DT2 *st2sum(DT2 *dtx, DT2 *dty);
DT3 *st3sum(DT3 *dtx, DT3 *dty);

int main() {

   DT3 fun1(DT3 *dt);
   DT3 fun2(DT3 dt);
   DT3 fun3(DT3 *dtx, DT3 *dty);
   DT3 *fun4(DT3 *dtx, DT3 *dty);

#ifdef CMPLX
   DT3 dt0 = {{2.0+I*2.0,4,6.0+I*6.0,8,10.0l+I*10.0l,12.0f+I*12.0f,14,16.0f+I*16.0f,
#else
   DT3 dt0 = {{createcomplex(2.0,2.0),4,createcomplex(6.0,6.0),8,createcomplexl(10.0l,10.0l),createcomplexf(12.0f,12.0f),14,createcomplexf(16.0f,16.0f),
#endif
#ifdef CMPLX
             {2,4.0+I*4.0,6,8.0f+I*8.0f,10.0l+I*10.0l,12.0+I*12.0,14,16.0f+I*16.0f}},
#else
             {2,createcomplex(4.0,4.0),6,createcomplexf(8.0f,8.0f),createcomplexl(10.0l,10.0l),createcomplex(12.0,12.0),14,createcomplexf(16.0f,16.0f)}},
#endif
#ifdef CMPLX
             2.0f+I*2.0f,4,6.0l+I*6.0l,8.0+I*8.0,10.0f+I*10.0f,12,14.0+I*14.0,16};
#else
             createcomplexf(2.0f,2.0f),4,createcomplexl(6.0l,6.0l),createcomplex(8.0,8.0),createcomplexf(10.0f,10.0f),12,createcomplex(14.0,14.0),16};
#endif

   DT3 dta, dtb, dtc, *dtp;

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

#ifdef CMPLX
   if ( dta.vdt2.var_a != 4.0+I*4.0 || dtb.vdt2.var_a != 4.0+I*4.0 ) exit(37);
#else
   if ( dta.vdt2.var_a != createcomplex(4.0,4.0) || dtb.vdt2.var_a != createcomplex(4.0,4.0) ) exit(37);
#endif
   if ( dta.vdt2.var_b != 6 || dtb.vdt2.var_b != 6 ) exit(39);
#ifdef CMPLX
   if ( dta.vdt2.var_c != 8.0+I*8.0 || dtb.vdt2.var_c != 8.0+I*8.0 ) exit(41);
#else
   if ( dta.vdt2.var_c != createcomplex(8.0,8.0) || dtb.vdt2.var_c != createcomplex(8.0,8.0) ) exit(41);
#endif
   if ( dta.vdt2.var_d != 10 || dtb.vdt2.var_d != 10 ) exit(43);
#ifdef CMPLX
   if ( dta.vdt2.var_e != 12.0l+I*12.0l || dtb.vdt2.var_e != 12.0l+I*12.0l ) exit(45);
#else
   if ( dta.vdt2.var_e != createcomplexl(12.0l,12.0l) || dtb.vdt2.var_e != createcomplexl(12.0l,12.0l) ) exit(45);
#endif
#ifdef CMPLX
   if ( dta.vdt2.var_f != 14.0f+I*14.0f || dtb.vdt2.var_f != 14.0f+I*14.0f ) exit(47);
#else
   if ( dta.vdt2.var_f != createcomplexf(14.0f,14.0f) || dtb.vdt2.var_f != createcomplexf(14.0f,14.0f) ) exit(47);
#endif
   if ( dta.vdt2.var_g != 16 || dtb.vdt2.var_g != 16 ) exit(49);
#ifdef CMPLX
   if ( dta.vdt2.var_h != 18.0f+I*18.0f || dtb.vdt2.var_h != 18.0f+I*18.0f ) exit(51);
#else
   if ( dta.vdt2.var_h != createcomplexf(18.0f,18.0f) || dtb.vdt2.var_h != createcomplexf(18.0f,18.0f) ) exit(51);
#endif

   if ( dta.vdt2.vdt1.var_a != 5 || 
                               dtb.vdt2.vdt1.var_a != 5 ) exit(53);
#ifdef CMPLX
   if ( dta.vdt2.vdt1.var_b != 7.0+I*7.0 || 
#else
   if ( dta.vdt2.vdt1.var_b != createcomplex(7.0,7.0) || 
#endif
#ifdef CMPLX
                               dta.vdt2.vdt1.var_b != 7.0+I*7.0 ) exit(55);
#else
                               dta.vdt2.vdt1.var_b != createcomplex(7.0,7.0) ) exit(55);
#endif
   if ( dta.vdt2.vdt1.var_c != 9 || 
                               dta.vdt2.vdt1.var_c != 9 ) exit(57);
#ifdef CMPLX
   if ( dta.vdt2.vdt1.var_d != 11.0f+I*11.0f || 
#else
   if ( dta.vdt2.vdt1.var_d != createcomplexf(11.0f,11.0f) || 
#endif
#ifdef CMPLX
                               dta.vdt2.vdt1.var_d != 11.0f+I*11.0f ) exit(59);
#else
                               dta.vdt2.vdt1.var_d != createcomplexf(11.0f,11.0f) ) exit(59);
#endif
#ifdef CMPLX
   if ( dta.vdt2.vdt1.var_e != 13.0l+I*13.0l || 
#else
   if ( dta.vdt2.vdt1.var_e != createcomplexl(13.0l,13.0l) || 
#endif
#ifdef CMPLX
                               dta.vdt2.vdt1.var_e != 13.0l+I*13.0l ) exit(61);
#else
                               dta.vdt2.vdt1.var_e != createcomplexl(13.0l,13.0l) ) exit(61);
#endif
#ifdef CMPLX
   if ( dta.vdt2.vdt1.var_f != 15.0+I*15.0 || 
#else
   if ( dta.vdt2.vdt1.var_f != createcomplex(15.0,15.0) || 
#endif
#ifdef CMPLX
                               dta.vdt2.vdt1.var_f != 15.0+I*15.0 ) exit(63);
#else
                               dta.vdt2.vdt1.var_f != createcomplex(15.0,15.0) ) exit(63);
#endif
   if ( dta.vdt2.vdt1.var_g != 17 || 
                               dta.vdt2.vdt1.var_g != 17 ) exit(65);
#ifdef CMPLX
   if ( dta.vdt2.vdt1.var_h != 19.0f+I*19.0f || 
#else
   if ( dta.vdt2.vdt1.var_h != createcomplexf(19.0f,19.0f) || 
#endif
#ifdef CMPLX
                               dta.vdt2.vdt1.var_h != 19.0f+I*19.0f ) exit(67);
#else
                               dta.vdt2.vdt1.var_h != createcomplexf(19.0f,19.0f) ) exit(67);
#endif

/* Test 2 */

   dta = dt0;

   dtb = fun2(dta);

#ifdef CMPLX
   if ( dta.var_a != 2.0f+I*2.0f || dtb.var_a != 3.0f+I*3.0f ) exit(69);
#else
   if ( dta.var_a != createcomplexf(2.0f,2.0f) || dtb.var_a != createcomplexf(3.0f,3.0f) ) exit(69);
#endif
   if ( dta.var_b != 4 || dtb.var_b != 5 ) exit(71);
#ifdef CMPLX
   if ( dta.var_c != 6.0l+I*6.0l || dtb.var_c != 7.0l+I*7.0l ) exit(73);
#else
   if ( dta.var_c != createcomplexl(6.0l,6.0l) || dtb.var_c != createcomplexl(7.0l,7.0l) ) exit(73);
#endif
#ifdef CMPLX
   if ( dta.var_d != 8.0+I*8.0 || dtb.var_d != 9.0+I*9.0 ) exit(75);
#else
   if ( dta.var_d != createcomplex(8.0,8.0) || dtb.var_d != createcomplex(9.0,9.0) ) exit(75);
#endif
#ifdef CMPLX
   if ( dta.var_e != 10.0f+I*10.0f || dtb.var_e != 11.0f+I*11.0f ) exit(77);
#else
   if ( dta.var_e != createcomplexf(10.0f,10.0f) || dtb.var_e != createcomplexf(11.0f,11.0f) ) exit(77);
#endif
   if ( dta.var_f != 12 || dtb.var_f != 13 ) exit(79);
#ifdef CMPLX
   if ( dta.var_g != 14.0+I*14.0 || dtb.var_g != 15.0+I*15.0 ) exit(81);
#else
   if ( dta.var_g != createcomplex(14.0,14.0) || dtb.var_g != createcomplex(15.0,15.0) ) exit(81);
#endif
   if ( dta.var_h != 16 || dtb.var_h != 17 ) exit(83);

#ifdef CMPLX
   if ( dta.vdt2.var_a != 2.0+I*2.0 || dtb.vdt2.var_a != 4.0+I*4.0 ) exit(85);
#else
   if ( dta.vdt2.var_a != createcomplex(2.0,2.0) || dtb.vdt2.var_a != createcomplex(4.0,4.0) ) exit(85);
#endif
   if ( dta.vdt2.var_b != 4 || dtb.vdt2.var_b != 6 ) exit(87);
#ifdef CMPLX
   if ( dta.vdt2.var_c != 6.0+I*6.0 || dtb.vdt2.var_c != 8.0+I*8.0 ) exit(89);
#else
   if ( dta.vdt2.var_c != createcomplex(6.0,6.0) || dtb.vdt2.var_c != createcomplex(8.0,8.0) ) exit(89);
#endif
   if ( dta.vdt2.var_d != 8 || dtb.vdt2.var_d != 10 ) exit(91);
#ifdef CMPLX
   if ( dta.vdt2.var_e != 10.0l+I*10.0l || dtb.vdt2.var_e != 12.0l+I*12.0l ) exit(93);
#else
   if ( dta.vdt2.var_e != createcomplexl(10.0l,10.0l) || dtb.vdt2.var_e != createcomplexl(12.0l,12.0l) ) exit(93);
#endif
#ifdef CMPLX
   if ( dta.vdt2.var_f != 12.0f+I*12.0f || dtb.vdt2.var_f != 14.0f+I*14.0f ) exit(95);
#else
   if ( dta.vdt2.var_f != createcomplexf(12.0f,12.0f) || dtb.vdt2.var_f != createcomplexf(14.0f,14.0f) ) exit(95);
#endif
   if ( dta.vdt2.var_g != 14 || dtb.vdt2.var_g != 16 ) exit(97);
#ifdef CMPLX
   if ( dta.vdt2.var_h != 16.0f+I*16.0f || dtb.vdt2.var_h != 18.0f+I*18.0f ) exit(99);
#else
   if ( dta.vdt2.var_h != createcomplexf(16.0f,16.0f) || dtb.vdt2.var_h != createcomplexf(18.0f,18.0f) ) exit(99);
#endif

   if ( dta.vdt2.vdt1.var_a != 2 || 
                               dtb.vdt2.vdt1.var_a != 5 ) exit(101);
#ifdef CMPLX
   if ( dta.vdt2.vdt1.var_b != 4.0+I*4.0 || 
#else
   if ( dta.vdt2.vdt1.var_b != createcomplex(4.0,4.0) || 
#endif
#ifdef CMPLX
                               dtb.vdt2.vdt1.var_b != 7.0+I*7.0 ) exit(103);
#else
                               dtb.vdt2.vdt1.var_b != createcomplex(7.0,7.0) ) exit(103);
#endif
   if ( dta.vdt2.vdt1.var_c != 6 || 
                               dtb.vdt2.vdt1.var_c != 9 ) exit(105);
#ifdef CMPLX
   if ( dta.vdt2.vdt1.var_d != 8.0f+I*8.0f || 
#else
   if ( dta.vdt2.vdt1.var_d != createcomplexf(8.0f,8.0f) || 
#endif
#ifdef CMPLX
                               dtb.vdt2.vdt1.var_d != 11.0f+I*11.0f ) exit(107);
#else
                               dtb.vdt2.vdt1.var_d != createcomplexf(11.0f,11.0f) ) exit(107);
#endif
#ifdef CMPLX
   if ( dta.vdt2.vdt1.var_e != 10.0l+I*10.0l || 
#else
   if ( dta.vdt2.vdt1.var_e != createcomplexl(10.0l,10.0l) || 
#endif
#ifdef CMPLX
                               dtb.vdt2.vdt1.var_e != 13.0l+I*13.0l ) exit(109);
#else
                               dtb.vdt2.vdt1.var_e != createcomplexl(13.0l,13.0l) ) exit(109);
#endif
#ifdef CMPLX
   if ( dta.vdt2.vdt1.var_f != 12.0+I*12.0 || 
#else
   if ( dta.vdt2.vdt1.var_f != createcomplex(12.0,12.0) || 
#endif
#ifdef CMPLX
                               dtb.vdt2.vdt1.var_f != 15.0+I*15.0 ) exit(111);
#else
                               dtb.vdt2.vdt1.var_f != createcomplex(15.0,15.0) ) exit(111);
#endif
   if ( dta.vdt2.vdt1.var_g != 14 || 
                               dtb.vdt2.vdt1.var_g != 17 ) exit(113);
#ifdef CMPLX
   if ( dta.vdt2.vdt1.var_h != 16.0f+I*16.0f || 
#else
   if ( dta.vdt2.vdt1.var_h != createcomplexf(16.0f,16.0f) || 
#endif
#ifdef CMPLX
                               dtb.vdt2.vdt1.var_h != 19.0f+I*19.0f ) exit(115);
#else
                               dtb.vdt2.vdt1.var_h != createcomplexf(19.0f,19.0f) ) exit(115);
#endif

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT3));
   dtp = st3sum(&dta,&dta);

   dtc = fun3(dtp,&dtb);

#ifdef CMPLX
   if ( dtb.var_a != 5.0f+I*5.0f || dtc.var_a != 5.0f+I*5.0f ) exit(117);
#else
   if ( dtb.var_a != createcomplexf(5.0f,5.0f) || dtc.var_a != createcomplexf(5.0f,5.0f) ) exit(117);
#endif
   if ( dtb.var_b != 9 || dtc.var_b != 9 ) exit(119);
#ifdef CMPLX
   if ( dtb.var_c != 13.0l+I*13.0l || dtc.var_c != 13.0l+I*13.0l ) exit(121);
#else
   if ( dtb.var_c != createcomplexl(13.0l,13.0l) || dtc.var_c != createcomplexl(13.0l,13.0l) ) exit(121);
#endif
#ifdef CMPLX
   if ( dtb.var_d != 17.0+I*17.0 || dtc.var_d != 17.0+I*17.0 ) exit(123);
#else
   if ( dtb.var_d != createcomplex(17.0,17.0) || dtc.var_d != createcomplex(17.0,17.0) ) exit(123);
#endif
#ifdef CMPLX
   if ( dtb.var_e != 21.0f+I*21.0f || dtc.var_e != 21.0f+I*21.0f ) exit(125);
#else
   if ( dtb.var_e != createcomplexf(21.0f,21.0f) || dtc.var_e != createcomplexf(21.0f,21.0f) ) exit(125);
#endif
   if ( dtb.var_f != 25 || dtc.var_f != 25 ) exit(127);
#ifdef CMPLX
   if ( dtb.var_g != 29.0+I*29.0 || dtc.var_g != 29.0+I*29.0 ) exit(129);
#else
   if ( dtb.var_g != createcomplex(29.0,29.0) || dtc.var_g != createcomplex(29.0,29.0) ) exit(129);
#endif
   if ( dtb.var_h != 33 || dtc.var_h != 33 ) exit(131);

#ifdef CMPLX
   if ( dtb.vdt2.var_a != 6.0+I*6.0 || dtc.vdt2.var_a != 6.0+I*6.0 ) exit(133);
#else
   if ( dtb.vdt2.var_a != createcomplex(6.0,6.0) || dtc.vdt2.var_a != createcomplex(6.0,6.0) ) exit(133);
#endif
   if ( dtb.vdt2.var_b != 10 || dtc.vdt2.var_b != 10 ) exit(135);
#ifdef CMPLX
   if ( dtb.vdt2.var_c != 14.0+I*14.0 || dtc.vdt2.var_c != 14.0+I*14.0 ) exit(137);
#else
   if ( dtb.vdt2.var_c != createcomplex(14.0,14.0) || dtc.vdt2.var_c != createcomplex(14.0,14.0) ) exit(137);
#endif
   if ( dtb.vdt2.var_d != 18 || dtc.vdt2.var_d != 18 ) exit(139);
#ifdef CMPLX
   if ( dtb.vdt2.var_e != 22.0l+I*22.0l || dtc.vdt2.var_e != 22.0l+I*22.0l ) exit(141);
#else
   if ( dtb.vdt2.var_e != createcomplexl(22.0l,22.0l) || dtc.vdt2.var_e != createcomplexl(22.0l,22.0l) ) exit(141);
#endif
#ifdef CMPLX
   if ( dtb.vdt2.var_f != 26.0f+I*26.0f || dtc.vdt2.var_f != 26.0f+I*26.0f ) exit(143);
#else
   if ( dtb.vdt2.var_f != createcomplexf(26.0f,26.0f) || dtc.vdt2.var_f != createcomplexf(26.0f,26.0f) ) exit(143);
#endif
   if ( dtb.vdt2.var_g != 30 || dtc.vdt2.var_g != 30 ) exit(145);
#ifdef CMPLX
   if ( dtb.vdt2.var_h != 34.0f+I*34.0f || dtc.vdt2.var_h != 34.0f+I*34.0f ) exit(147);
#else
   if ( dtb.vdt2.var_h != createcomplexf(34.0f,34.0f) || dtc.vdt2.var_h != createcomplexf(34.0f,34.0f) ) exit(147);
#endif

   if ( dtb.vdt2.vdt1.var_a != 7 || 
                              dtc.vdt2.vdt1.var_a != 7 ) exit(149);
#ifdef CMPLX
   if ( dtb.vdt2.vdt1.var_b != 11.0+I*11.0 || 
#else
   if ( dtb.vdt2.vdt1.var_b != createcomplex(11.0,11.0) || 
#endif
#ifdef CMPLX
                              dtc.vdt2.vdt1.var_b != 11.0+I*11.0 ) exit(151);
#else
                              dtc.vdt2.vdt1.var_b != createcomplex(11.0,11.0) ) exit(151);
#endif
   if ( dtb.vdt2.vdt1.var_c != 15 || 
                              dtc.vdt2.vdt1.var_c != 15 ) exit(153);
#ifdef CMPLX
   if ( dtb.vdt2.vdt1.var_d != 19.0f+I*19.0f || 
#else
   if ( dtb.vdt2.vdt1.var_d != createcomplexf(19.0f,19.0f) || 
#endif
#ifdef CMPLX
                              dtc.vdt2.vdt1.var_d != 19.0f+I*19.0f ) exit(155);
#else
                              dtc.vdt2.vdt1.var_d != createcomplexf(19.0f,19.0f) ) exit(155);
#endif
#ifdef CMPLX
   if ( dtb.vdt2.vdt1.var_e != 23.0l+I*23.0l || 
#else
   if ( dtb.vdt2.vdt1.var_e != createcomplexl(23.0l,23.0l) || 
#endif
#ifdef CMPLX
                              dtc.vdt2.vdt1.var_e != 23.0l+I*23.0l ) exit(157);
#else
                              dtc.vdt2.vdt1.var_e != createcomplexl(23.0l,23.0l) ) exit(157);
#endif
#ifdef CMPLX
   if ( dtb.vdt2.vdt1.var_f != 27.0+I*27.0 || 
#else
   if ( dtb.vdt2.vdt1.var_f != createcomplex(27.0,27.0) || 
#endif
#ifdef CMPLX
                              dtc.vdt2.vdt1.var_f != 27.0+I*27.0 ) exit(159);
#else
                              dtc.vdt2.vdt1.var_f != createcomplex(27.0,27.0) ) exit(159);
#endif
   if ( dtb.vdt2.vdt1.var_g != 31 || 
                              dtc.vdt2.vdt1.var_g != 31 ) exit(161);
#ifdef CMPLX
   if ( dtb.vdt2.vdt1.var_h != 35.0f+I*35.0f || 
#else
   if ( dtb.vdt2.vdt1.var_h != createcomplexf(35.0f,35.0f) || 
#endif
#ifdef CMPLX
                              dtc.vdt2.vdt1.var_h != 35.0f+I*35.0f ) exit(163);
#else
                              dtc.vdt2.vdt1.var_h != createcomplexf(35.0f,35.0f) ) exit(163);
#endif

/* Test 4 */

   dta = dt0;
   dtp = malloc(sizeof(DT3));
   dtp = st3sum(&dta,&dta);

   dtp = fun4(dtp,&dtb);

#ifdef CMPLX
   if ( dtb.var_a != 5.0f+I*5.0f || dtp->var_a != 5.0f+I*5.0f ) exit(165);
#else
   if ( dtb.var_a != createcomplexf(5.0f,5.0f) || dtp->var_a != createcomplexf(5.0f,5.0f) ) exit(165);
#endif
   if ( dtb.var_b != 9 || dtp->var_b != 9 ) exit(167);
#ifdef CMPLX
   if ( dtb.var_c != 13.0l+I*13.0l || dtp->var_c != 13.0l+I*13.0l ) exit(169);
#else
   if ( dtb.var_c != createcomplexl(13.0l,13.0l) || dtp->var_c != createcomplexl(13.0l,13.0l) ) exit(169);
#endif
#ifdef CMPLX
   if ( dtb.var_d != 17.0+I*17.0 || dtp->var_d != 17.0+I*17.0 ) exit(171);
#else
   if ( dtb.var_d != createcomplex(17.0,17.0) || dtp->var_d != createcomplex(17.0,17.0) ) exit(171);
#endif
#ifdef CMPLX
   if ( dtb.var_e != 21.0f+I*21.0f || dtp->var_e != 21.0f+I*21.0f ) exit(173);
#else
   if ( dtb.var_e != createcomplexf(21.0f,21.0f) || dtp->var_e != createcomplexf(21.0f,21.0f) ) exit(173);
#endif
   if ( dtb.var_f != 25 || dtp->var_f != 25 ) exit(175);
#ifdef CMPLX
   if ( dtb.var_g != 29.0+I*29.0 || dtp->var_g != 29.0+I*29.0 ) exit(177);
#else
   if ( dtb.var_g != createcomplex(29.0,29.0) || dtp->var_g != createcomplex(29.0,29.0) ) exit(177);
#endif
   if ( dtb.var_h != 33 || dtp->var_h != 33 ) exit(179);

#ifdef CMPLX
   if ( dtb.vdt2.var_a != 6.0+I*6.0 || dtp->vdt2.var_a != 6.0+I*6.0 ) exit(181);
#else
   if ( dtb.vdt2.var_a != createcomplex(6.0,6.0) || dtp->vdt2.var_a != createcomplex(6.0,6.0) ) exit(181);
#endif
   if ( dtb.vdt2.var_b != 10 || dtp->vdt2.var_b != 10 ) exit(183);
#ifdef CMPLX
   if ( dtb.vdt2.var_c != 14.0+I*14.0 || dtp->vdt2.var_c != 14.0+I*14.0 ) exit(185);
#else
   if ( dtb.vdt2.var_c != createcomplex(14.0,14.0) || dtp->vdt2.var_c != createcomplex(14.0,14.0) ) exit(185);
#endif
   if ( dtb.vdt2.var_d != 18 || dtp->vdt2.var_d != 18 ) exit(187);
#ifdef CMPLX
   if ( dtb.vdt2.var_e != 22.0l+I*22.0l || dtp->vdt2.var_e != 22.0l+I*22.0l ) exit(189);
#else
   if ( dtb.vdt2.var_e != createcomplexl(22.0l,22.0l) || dtp->vdt2.var_e != createcomplexl(22.0l,22.0l) ) exit(189);
#endif
#ifdef CMPLX
   if ( dtb.vdt2.var_f != 26.0f+I*26.0f || dtp->vdt2.var_f != 26.0f+I*26.0f ) exit(191);
#else
   if ( dtb.vdt2.var_f != createcomplexf(26.0f,26.0f) || dtp->vdt2.var_f != createcomplexf(26.0f,26.0f) ) exit(191);
#endif
   if ( dtb.vdt2.var_g != 30 || dtp->vdt2.var_g != 30 ) exit(193);
#ifdef CMPLX
   if ( dtb.vdt2.var_h != 34.0f+I*34.0f || dtp->vdt2.var_h != 34.0f+I*34.0f ) exit(195);
#else
   if ( dtb.vdt2.var_h != createcomplexf(34.0f,34.0f) || dtp->vdt2.var_h != createcomplexf(34.0f,34.0f) ) exit(195);
#endif

   if ( dtb.vdt2.vdt1.var_a != 7 || 
                              dtp->vdt2.vdt1.var_a != 7 ) exit(197);
#ifdef CMPLX
   if ( dtb.vdt2.vdt1.var_b != 11.0+I*11.0 || 
#else
   if ( dtb.vdt2.vdt1.var_b != createcomplex(11.0,11.0) || 
#endif
#ifdef CMPLX
                              dtp->vdt2.vdt1.var_b != 11.0+I*11.0 ) exit(199);
#else
                              dtp->vdt2.vdt1.var_b != createcomplex(11.0,11.0) ) exit(199);
#endif
   if ( dtb.vdt2.vdt1.var_c != 15 || 
                              dtp->vdt2.vdt1.var_c != 15 ) exit(201);
#ifdef CMPLX
   if ( dtb.vdt2.vdt1.var_d != 19.0f+I*19.0f || 
#else
   if ( dtb.vdt2.vdt1.var_d != createcomplexf(19.0f,19.0f) || 
#endif
#ifdef CMPLX
                              dtp->vdt2.vdt1.var_d != 19.0f+I*19.0f ) exit(203);
#else
                              dtp->vdt2.vdt1.var_d != createcomplexf(19.0f,19.0f) ) exit(203);
#endif
#ifdef CMPLX
   if ( dtb.vdt2.vdt1.var_e != 23.0l+I*23.0l || 
#else
   if ( dtb.vdt2.vdt1.var_e != createcomplexl(23.0l,23.0l) || 
#endif
#ifdef CMPLX
                              dtp->vdt2.vdt1.var_e != 23.0l+I*23.0l ) exit(205);
#else
                              dtp->vdt2.vdt1.var_e != createcomplexl(23.0l,23.0l) ) exit(205);
#endif
#ifdef CMPLX
   if ( dtb.vdt2.vdt1.var_f != 27.0+I*27.0 || 
#else
   if ( dtb.vdt2.vdt1.var_f != createcomplex(27.0,27.0) || 
#endif
#ifdef CMPLX
                              dtp->vdt2.vdt1.var_f != 27.0+I*27.0 ) exit(207);
#else
                              dtp->vdt2.vdt1.var_f != createcomplex(27.0,27.0) ) exit(207);
#endif
   if ( dtb.vdt2.vdt1.var_g != 31 || 
                              dtp->vdt2.vdt1.var_g != 31 ) exit(209);
#ifdef CMPLX
   if ( dtb.vdt2.vdt1.var_h != 35.0f+I*35.0f || 
#else
   if ( dtb.vdt2.vdt1.var_h != createcomplexf(35.0f,35.0f) || 
#endif
#ifdef CMPLX
                              dtp->vdt2.vdt1.var_h != 35.0f+I*35.0f ) exit(211);
#else
                              dtp->vdt2.vdt1.var_h != createcomplexf(35.0f,35.0f) ) exit(211);
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
