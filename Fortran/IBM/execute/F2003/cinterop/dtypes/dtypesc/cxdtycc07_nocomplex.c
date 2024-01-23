
#include <stdio.h>
#include <stdlib.h>

   #include "cmplx.h"

#include <inttypes.h>
#include <stddef.h>

  #define int_fast16_t short

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

   DT3 dt0 = {{createcomplex(2.0,2.0),4,createcomplex(6.0,6.0),8,createcomplexl(10.0l,10.0l),createcomplexf(12.0f,12.0f),14,createcomplexf(16.0f,16.0f),
             {2,createcomplex(4.0,4.0),6,createcomplexf(8.0f,8.0f),createcomplexl(10.0l,10.0l),createcomplex(12.0,12.0),14,createcomplexf(16.0f,16.0f)}},
             createcomplexf(2.0f,2.0f),4,createcomplexl(6.0l,6.0l),createcomplex(8.0,8.0),createcomplexf(10.0f,10.0f),12,createcomplex(14.0,14.0),16};

   DT3 dta, dtb, dtc, *dtp;

/* Test 1 */

   dta = dt0;

   dtb = fun1(&dta);

   if ( dta.var_a != createcomplexf(3.0f,3.0f) || dtb.var_a != createcomplexf(3.0f,3.0f) ) exit(21);
   if ( dta.var_b != 5 || dtb.var_b != 5 ) exit(23);
   if ( dta.var_c != createcomplexl(7.0l,7.0l) || dtb.var_c != createcomplexl(7.0l,7.0l) ) exit(25);
   if ( dta.var_d != createcomplex(9.0,9.0) || dtb.var_d != createcomplex(9.0,9.0) ) exit(27);
   if ( dta.var_e != createcomplexf(11.0f,11.0f) || dtb.var_e != createcomplexf(11.0f,11.0f) ) exit(29);
   if ( dta.var_f != 13 || dtb.var_f != 13 ) exit(31);
   if ( dta.var_g != createcomplex(15.0,15.0) || dtb.var_g != createcomplex(15.0,15.0) ) exit(33);
   if ( dta.var_h != 17 || dtb.var_h != 17 ) exit(35);

   if ( dta.vdt2.var_a != createcomplex(4.0,4.0) || dtb.vdt2.var_a != createcomplex(4.0,4.0) ) exit(37);
   if ( dta.vdt2.var_b != 6 || dtb.vdt2.var_b != 6 ) exit(39);
   if ( dta.vdt2.var_c != createcomplex(8.0,8.0) || dtb.vdt2.var_c != createcomplex(8.0,8.0) ) exit(41);
   if ( dta.vdt2.var_d != 10 || dtb.vdt2.var_d != 10 ) exit(43);
   if ( dta.vdt2.var_e != createcomplexl(12.0l,12.0l) || dtb.vdt2.var_e != createcomplexl(12.0l,12.0l) ) exit(45);
   if ( dta.vdt2.var_f != createcomplexf(14.0f,14.0f) || dtb.vdt2.var_f != createcomplexf(14.0f,14.0f) ) exit(47);
   if ( dta.vdt2.var_g != 16 || dtb.vdt2.var_g != 16 ) exit(49);
   if ( dta.vdt2.var_h != createcomplexf(18.0f,18.0f) || dtb.vdt2.var_h != createcomplexf(18.0f,18.0f) ) exit(51);

   if ( dta.vdt2.vdt1.var_a != 5 || 
                               dtb.vdt2.vdt1.var_a != 5 ) exit(53);
   if ( dta.vdt2.vdt1.var_b != createcomplex(7.0,7.0) || 
                               dta.vdt2.vdt1.var_b != createcomplex(7.0,7.0) ) exit(55);
   if ( dta.vdt2.vdt1.var_c != 9 || 
                               dta.vdt2.vdt1.var_c != 9 ) exit(57);
   if ( dta.vdt2.vdt1.var_d != createcomplexf(11.0f,11.0f) || 
                               dta.vdt2.vdt1.var_d != createcomplexf(11.0f,11.0f) ) exit(59);
   if ( dta.vdt2.vdt1.var_e != createcomplexl(13.0l,13.0l) || 
                               dta.vdt2.vdt1.var_e != createcomplexl(13.0l,13.0l) ) exit(61);
   if ( dta.vdt2.vdt1.var_f != createcomplex(15.0,15.0) || 
                               dta.vdt2.vdt1.var_f != createcomplex(15.0,15.0) ) exit(63);
   if ( dta.vdt2.vdt1.var_g != 17 || 
                               dta.vdt2.vdt1.var_g != 17 ) exit(65);
   if ( dta.vdt2.vdt1.var_h != createcomplexf(19.0f,19.0f) || 
                               dta.vdt2.vdt1.var_h != createcomplexf(19.0f,19.0f) ) exit(67);

/* Test 2 */

   dta = dt0;

   dtb = fun2(dta);

   if ( dta.var_a != createcomplexf(2.0f,2.0f) || dtb.var_a != createcomplexf(3.0f,3.0f) ) exit(69);
   if ( dta.var_b != 4 || dtb.var_b != 5 ) exit(71);
   if ( dta.var_c != createcomplexl(6.0l,6.0l) || dtb.var_c != createcomplexl(7.0l,7.0l) ) exit(73);
   if ( dta.var_d != createcomplex(8.0,8.0) || dtb.var_d != createcomplex(9.0,9.0) ) exit(75);
   if ( dta.var_e != createcomplexf(10.0f,10.0f) || dtb.var_e != createcomplexf(11.0f,11.0f) ) exit(77);
   if ( dta.var_f != 12 || dtb.var_f != 13 ) exit(79);
   if ( dta.var_g != createcomplex(14.0,14.0) || dtb.var_g != createcomplex(15.0,15.0) ) exit(81);
   if ( dta.var_h != 16 || dtb.var_h != 17 ) exit(83);

   if ( dta.vdt2.var_a != createcomplex(2.0,2.0) || dtb.vdt2.var_a != createcomplex(4.0,4.0) ) exit(85);
   if ( dta.vdt2.var_b != 4 || dtb.vdt2.var_b != 6 ) exit(87);
   if ( dta.vdt2.var_c != createcomplex(6.0,6.0) || dtb.vdt2.var_c != createcomplex(8.0,8.0) ) exit(89);
   if ( dta.vdt2.var_d != 8 || dtb.vdt2.var_d != 10 ) exit(91);
   if ( dta.vdt2.var_e != createcomplexl(10.0l,10.0l) || dtb.vdt2.var_e != createcomplexl(12.0l,12.0l) ) exit(93);
   if ( dta.vdt2.var_f != createcomplexf(12.0f,12.0f) || dtb.vdt2.var_f != createcomplexf(14.0f,14.0f) ) exit(95);
   if ( dta.vdt2.var_g != 14 || dtb.vdt2.var_g != 16 ) exit(97);
   if ( dta.vdt2.var_h != createcomplexf(16.0f,16.0f) || dtb.vdt2.var_h != createcomplexf(18.0f,18.0f) ) exit(99);

   if ( dta.vdt2.vdt1.var_a != 2 || 
                               dtb.vdt2.vdt1.var_a != 5 ) exit(101);
   if ( dta.vdt2.vdt1.var_b != createcomplex(4.0,4.0) || 
                               dtb.vdt2.vdt1.var_b != createcomplex(7.0,7.0) ) exit(103);
   if ( dta.vdt2.vdt1.var_c != 6 || 
                               dtb.vdt2.vdt1.var_c != 9 ) exit(105);
   if ( dta.vdt2.vdt1.var_d != createcomplexf(8.0f,8.0f) || 
                               dtb.vdt2.vdt1.var_d != createcomplexf(11.0f,11.0f) ) exit(107);
   if ( dta.vdt2.vdt1.var_e != createcomplexl(10.0l,10.0l) || 
                               dtb.vdt2.vdt1.var_e != createcomplexl(13.0l,13.0l) ) exit(109);
   if ( dta.vdt2.vdt1.var_f != createcomplex(12.0,12.0) || 
                               dtb.vdt2.vdt1.var_f != createcomplex(15.0,15.0) ) exit(111);
   if ( dta.vdt2.vdt1.var_g != 14 || 
                               dtb.vdt2.vdt1.var_g != 17 ) exit(113);
   if ( dta.vdt2.vdt1.var_h != createcomplexf(16.0f,16.0f) || 
                               dtb.vdt2.vdt1.var_h != createcomplexf(19.0f,19.0f) ) exit(115);

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT3));
   dtp = st3sum(&dta,&dta);

   dtc = fun3(dtp,&dtb);

   if ( dtb.var_a != createcomplexf(5.0f,5.0f) || dtc.var_a != createcomplexf(5.0f,5.0f) ) exit(117);
   if ( dtb.var_b != 9 || dtc.var_b != 9 ) exit(119);
   if ( dtb.var_c != createcomplexl(13.0l,13.0l) || dtc.var_c != createcomplexl(13.0l,13.0l) ) exit(121);
   if ( dtb.var_d != createcomplex(17.0,17.0) || dtc.var_d != createcomplex(17.0,17.0) ) exit(123);
   if ( dtb.var_e != createcomplexf(21.0f,21.0f) || dtc.var_e != createcomplexf(21.0f,21.0f) ) exit(125);
   if ( dtb.var_f != 25 || dtc.var_f != 25 ) exit(127);
   if ( dtb.var_g != createcomplex(29.0,29.0) || dtc.var_g != createcomplex(29.0,29.0) ) exit(129);
   if ( dtb.var_h != 33 || dtc.var_h != 33 ) exit(131);

   if ( dtb.vdt2.var_a != createcomplex(6.0,6.0) || dtc.vdt2.var_a != createcomplex(6.0,6.0) ) exit(133);
   if ( dtb.vdt2.var_b != 10 || dtc.vdt2.var_b != 10 ) exit(135);
   if ( dtb.vdt2.var_c != createcomplex(14.0,14.0) || dtc.vdt2.var_c != createcomplex(14.0,14.0) ) exit(137);
   if ( dtb.vdt2.var_d != 18 || dtc.vdt2.var_d != 18 ) exit(139);
   if ( dtb.vdt2.var_e != createcomplexl(22.0l,22.0l) || dtc.vdt2.var_e != createcomplexl(22.0l,22.0l) ) exit(141);
   if ( dtb.vdt2.var_f != createcomplexf(26.0f,26.0f) || dtc.vdt2.var_f != createcomplexf(26.0f,26.0f) ) exit(143);
   if ( dtb.vdt2.var_g != 30 || dtc.vdt2.var_g != 30 ) exit(145);
   if ( dtb.vdt2.var_h != createcomplexf(34.0f,34.0f) || dtc.vdt2.var_h != createcomplexf(34.0f,34.0f) ) exit(147);

   if ( dtb.vdt2.vdt1.var_a != 7 || 
                              dtc.vdt2.vdt1.var_a != 7 ) exit(149);
   if ( dtb.vdt2.vdt1.var_b != createcomplex(11.0,11.0) || 
                              dtc.vdt2.vdt1.var_b != createcomplex(11.0,11.0) ) exit(151);
   if ( dtb.vdt2.vdt1.var_c != 15 || 
                              dtc.vdt2.vdt1.var_c != 15 ) exit(153);
   if ( dtb.vdt2.vdt1.var_d != createcomplexf(19.0f,19.0f) || 
                              dtc.vdt2.vdt1.var_d != createcomplexf(19.0f,19.0f) ) exit(155);
   if ( dtb.vdt2.vdt1.var_e != createcomplexl(23.0l,23.0l) || 
                              dtc.vdt2.vdt1.var_e != createcomplexl(23.0l,23.0l) ) exit(157);
   if ( dtb.vdt2.vdt1.var_f != createcomplex(27.0,27.0) || 
                              dtc.vdt2.vdt1.var_f != createcomplex(27.0,27.0) ) exit(159);
   if ( dtb.vdt2.vdt1.var_g != 31 || 
                              dtc.vdt2.vdt1.var_g != 31 ) exit(161);
   if ( dtb.vdt2.vdt1.var_h != createcomplexf(35.0f,35.0f) || 
                              dtc.vdt2.vdt1.var_h != createcomplexf(35.0f,35.0f) ) exit(163);

/* Test 4 */

   dta = dt0;
   dtp = malloc(sizeof(DT3));
   dtp = st3sum(&dta,&dta);

   dtp = fun4(dtp,&dtb);

   if ( dtb.var_a != createcomplexf(5.0f,5.0f) || dtp->var_a != createcomplexf(5.0f,5.0f) ) exit(165);
   if ( dtb.var_b != 9 || dtp->var_b != 9 ) exit(167);
   if ( dtb.var_c != createcomplexl(13.0l,13.0l) || dtp->var_c != createcomplexl(13.0l,13.0l) ) exit(169);
   if ( dtb.var_d != createcomplex(17.0,17.0) || dtp->var_d != createcomplex(17.0,17.0) ) exit(171);
   if ( dtb.var_e != createcomplexf(21.0f,21.0f) || dtp->var_e != createcomplexf(21.0f,21.0f) ) exit(173);
   if ( dtb.var_f != 25 || dtp->var_f != 25 ) exit(175);
   if ( dtb.var_g != createcomplex(29.0,29.0) || dtp->var_g != createcomplex(29.0,29.0) ) exit(177);
   if ( dtb.var_h != 33 || dtp->var_h != 33 ) exit(179);

   if ( dtb.vdt2.var_a != createcomplex(6.0,6.0) || dtp->vdt2.var_a != createcomplex(6.0,6.0) ) exit(181);
   if ( dtb.vdt2.var_b != 10 || dtp->vdt2.var_b != 10 ) exit(183);
   if ( dtb.vdt2.var_c != createcomplex(14.0,14.0) || dtp->vdt2.var_c != createcomplex(14.0,14.0) ) exit(185);
   if ( dtb.vdt2.var_d != 18 || dtp->vdt2.var_d != 18 ) exit(187);
   if ( dtb.vdt2.var_e != createcomplexl(22.0l,22.0l) || dtp->vdt2.var_e != createcomplexl(22.0l,22.0l) ) exit(189);
   if ( dtb.vdt2.var_f != createcomplexf(26.0f,26.0f) || dtp->vdt2.var_f != createcomplexf(26.0f,26.0f) ) exit(191);
   if ( dtb.vdt2.var_g != 30 || dtp->vdt2.var_g != 30 ) exit(193);
   if ( dtb.vdt2.var_h != createcomplexf(34.0f,34.0f) || dtp->vdt2.var_h != createcomplexf(34.0f,34.0f) ) exit(195);

   if ( dtb.vdt2.vdt1.var_a != 7 || 
                              dtp->vdt2.vdt1.var_a != 7 ) exit(197);
   if ( dtb.vdt2.vdt1.var_b != createcomplex(11.0,11.0) || 
                              dtp->vdt2.vdt1.var_b != createcomplex(11.0,11.0) ) exit(199);
   if ( dtb.vdt2.vdt1.var_c != 15 || 
                              dtp->vdt2.vdt1.var_c != 15 ) exit(201);
   if ( dtb.vdt2.vdt1.var_d != createcomplexf(19.0f,19.0f) || 
                              dtp->vdt2.vdt1.var_d != createcomplexf(19.0f,19.0f) ) exit(203);
   if ( dtb.vdt2.vdt1.var_e != createcomplexl(23.0l,23.0l) || 
                              dtp->vdt2.vdt1.var_e != createcomplexl(23.0l,23.0l) ) exit(205);
   if ( dtb.vdt2.vdt1.var_f != createcomplex(27.0,27.0) || 
                              dtp->vdt2.vdt1.var_f != createcomplex(27.0,27.0) ) exit(207);
   if ( dtb.vdt2.vdt1.var_g != 31 || 
                              dtp->vdt2.vdt1.var_g != 31 ) exit(209);
   if ( dtb.vdt2.vdt1.var_h != createcomplexf(35.0f,35.0f) || 
                              dtp->vdt2.vdt1.var_h != createcomplexf(35.0f,35.0f) ) exit(211);

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
