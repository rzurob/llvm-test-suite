
#include <stdio.h>
#include <stdlib.h>

   #include <inttypes.h>
   #include <stddef.h>


typedef struct dt1 DT1;
typedef struct dt2 DT2;
typedef struct dt3 DT3;

struct dt1 {
   char var_a;
   int var_b;
   short var_c;
   long long var_d;
   long var_e;
   short var_f;
   long long var_g;
   int var_h;
};

struct dt2 {
   int16_t var_a;
   int_fast8_t var_b;
   int64_t var_c;
   int8_t var_d;
   int_fast32_t var_e;
   int_fast16_t var_f;
   int32_t var_g;
   int_fast64_t var_h;
   DT1 vdt1;
};

struct dt3 {
   DT2 vdt2;
   size_t var_a;
   int_least8_t var_b;
   int_least64_t var_c;
   signed char var_d;
   intptr_t var_e;
   int_least16_t var_f;
   intmax_t var_g;
   int_least32_t var_h;
};

DT1 *st1sum(DT1 *dtx, DT1 *dty);
DT2 *st2sum(DT2 *dtx, DT2 *dty);
DT3 *st3sum(DT3 *dtx, DT3 *dty);

int main() {

   DT3 fun1(DT3 *dt);
   DT3 fun2(DT3 dt);
   DT3 fun3(DT3 *dtx, DT3 *dty);
   DT3 *fun4(DT3 *dtx, DT3 *dty);

   DT3 dt0 = {{2,4,6,8,10,12,14,16,
             {2,4,6,8,10,12,14,16}},
             2,4,6,8,10,12,14,16};

   DT3 dta, dtb, dtc, *dtp;

/* Test 1 */

   dta = dt0;

   dtb = fun1(&dta);

   if ( dta.var_a != 3 || dtb.var_a != 3 ) exit(21);
   if ( dta.var_b != 5 || dtb.var_b != 5 ) exit(23);
   if ( dta.var_c != 7 || dtb.var_c != 7 ) exit(25);
   if ( dta.var_d != 9 || dtb.var_d != 9 ) exit(27);
   if ( dta.var_e != 11 || dtb.var_e != 11 ) exit(29);
   if ( dta.var_f != 13 || dtb.var_f != 13 ) exit(31);
   if ( dta.var_g != 15 || dtb.var_g != 15 ) exit(33);
   if ( dta.var_h != 17 || dtb.var_h != 17 ) exit(35);

   if ( dta.vdt2.var_a != 4 || dtb.vdt2.var_a != 4 ) exit(37);
   if ( dta.vdt2.var_b != 6 || dtb.vdt2.var_b != 6 ) exit(39);
   if ( dta.vdt2.var_c != 8 || dtb.vdt2.var_c != 8 ) exit(41);
   if ( dta.vdt2.var_d != 10 || dtb.vdt2.var_d != 10 ) exit(43);
   if ( dta.vdt2.var_e != 12 || dtb.vdt2.var_e != 12 ) exit(45);
   if ( dta.vdt2.var_f != 14 || dtb.vdt2.var_f != 14 ) exit(47);
   if ( dta.vdt2.var_g != 16 || dtb.vdt2.var_g != 16 ) exit(49);
   if ( dta.vdt2.var_h != 18 || dtb.vdt2.var_h != 18 ) exit(51);

   if ( dta.vdt2.vdt1.var_a != 5 || 
                               dtb.vdt2.vdt1.var_a != 5 ) exit(53);
   if ( dta.vdt2.vdt1.var_b != 7 || 
                               dta.vdt2.vdt1.var_b != 7 ) exit(55);
   if ( dta.vdt2.vdt1.var_c != 9 || 
                               dta.vdt2.vdt1.var_c != 9 ) exit(57);
   if ( dta.vdt2.vdt1.var_d != 11 || 
                               dta.vdt2.vdt1.var_d != 11 ) exit(59);
   if ( dta.vdt2.vdt1.var_e != 13 || 
                               dta.vdt2.vdt1.var_e != 13 ) exit(61);
   if ( dta.vdt2.vdt1.var_f != 15 || 
                               dta.vdt2.vdt1.var_f != 15 ) exit(63);
   if ( dta.vdt2.vdt1.var_g != 17 || 
                               dta.vdt2.vdt1.var_g != 17 ) exit(65);
   if ( dta.vdt2.vdt1.var_h != 19 || 
                               dta.vdt2.vdt1.var_h != 19 ) exit(67);

/* Test 2 */

   dta = dt0;

   dtb = fun2(dta);

   if ( dta.var_a != 2 || dtb.var_a != 3 ) exit(69);
   if ( dta.var_b != 4 || dtb.var_b != 5 ) exit(71);
   if ( dta.var_c != 6 || dtb.var_c != 7 ) exit(73);
   if ( dta.var_d != 8 || dtb.var_d != 9 ) exit(75);
   if ( dta.var_e != 10 || dtb.var_e != 11 ) exit(77);
   if ( dta.var_f != 12 || dtb.var_f != 13 ) exit(79);
   if ( dta.var_g != 14 || dtb.var_g != 15 ) exit(81);
   if ( dta.var_h != 16 || dtb.var_h != 17 ) exit(83);

   if ( dta.vdt2.var_a != 2 || dtb.vdt2.var_a != 4 ) exit(85);
   if ( dta.vdt2.var_b != 4 || dtb.vdt2.var_b != 6 ) exit(87);
   if ( dta.vdt2.var_c != 6 || dtb.vdt2.var_c != 8 ) exit(89);
   if ( dta.vdt2.var_d != 8 || dtb.vdt2.var_d != 10 ) exit(91);
   if ( dta.vdt2.var_e != 10 || dtb.vdt2.var_e != 12 ) exit(93);
   if ( dta.vdt2.var_f != 12 || dtb.vdt2.var_f != 14 ) exit(95);
   if ( dta.vdt2.var_g != 14 || dtb.vdt2.var_g != 16 ) exit(97);
   if ( dta.vdt2.var_h != 16 || dtb.vdt2.var_h != 18 ) exit(99);

   if ( dta.vdt2.vdt1.var_a != 2 || 
                               dtb.vdt2.vdt1.var_a != 5 ) exit(101);
   if ( dta.vdt2.vdt1.var_b != 4 || 
                               dtb.vdt2.vdt1.var_b != 7 ) exit(103);
   if ( dta.vdt2.vdt1.var_c != 6 || 
                               dtb.vdt2.vdt1.var_c != 9 ) exit(105);
   if ( dta.vdt2.vdt1.var_d != 8 || 
                               dtb.vdt2.vdt1.var_d != 11 ) exit(107);
   if ( dta.vdt2.vdt1.var_e != 10 || 
                               dtb.vdt2.vdt1.var_e != 13 ) exit(109);
   if ( dta.vdt2.vdt1.var_f != 12 || 
                               dtb.vdt2.vdt1.var_f != 15 ) exit(111);
   if ( dta.vdt2.vdt1.var_g != 14 || 
                               dtb.vdt2.vdt1.var_g != 17 ) exit(113);
   if ( dta.vdt2.vdt1.var_h != 16 || 
                               dtb.vdt2.vdt1.var_h != 19 ) exit(115);

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT3));
   dtp = st3sum(&dta,&dta);

   dtc = fun3(dtp,&dtb);

   if ( dtb.var_a != 5 || dtc.var_a != 5 ) exit(117);
   if ( dtb.var_b != 9 || dtc.var_b != 9 ) exit(119);
   if ( dtb.var_c != 13 || dtc.var_c != 13 ) exit(121);
   if ( dtb.var_d != 17 || dtc.var_d != 17 ) exit(123);
   if ( dtb.var_e != 21 || dtc.var_e != 21 ) exit(125);
   if ( dtb.var_f != 25 || dtc.var_f != 25 ) exit(127);
   if ( dtb.var_g != 29 || dtc.var_g != 29 ) exit(129);
   if ( dtb.var_h != 33 || dtc.var_h != 33 ) exit(131);

   if ( dtb.vdt2.var_a != 6 || dtc.vdt2.var_a != 6 ) exit(133);
   if ( dtb.vdt2.var_b != 10 || dtc.vdt2.var_b != 10 ) exit(135);
   if ( dtb.vdt2.var_c != 14 || dtc.vdt2.var_c != 14 ) exit(137);
   if ( dtb.vdt2.var_d != 18 || dtc.vdt2.var_d != 18 ) exit(139);
   if ( dtb.vdt2.var_e != 22 || dtc.vdt2.var_e != 22 ) exit(141);
   if ( dtb.vdt2.var_f != 26 || dtc.vdt2.var_f != 26 ) exit(143);
   if ( dtb.vdt2.var_g != 30 || dtc.vdt2.var_g != 30 ) exit(145);
   if ( dtb.vdt2.var_h != 34 || dtc.vdt2.var_h != 34 ) exit(147);

   if ( dtb.vdt2.vdt1.var_a != 7 || 
                              dtc.vdt2.vdt1.var_a != 7 ) exit(149);
   if ( dtb.vdt2.vdt1.var_b != 11 || 
                              dtc.vdt2.vdt1.var_b != 11 ) exit(151);
   if ( dtb.vdt2.vdt1.var_c != 15 || 
                              dtc.vdt2.vdt1.var_c != 15 ) exit(153);
   if ( dtb.vdt2.vdt1.var_d != 19 || 
                              dtc.vdt2.vdt1.var_d != 19 ) exit(155);
   if ( dtb.vdt2.vdt1.var_e != 23 || 
                              dtc.vdt2.vdt1.var_e != 23 ) exit(157);
   if ( dtb.vdt2.vdt1.var_f != 27 || 
                              dtc.vdt2.vdt1.var_f != 27 ) exit(159);
   if ( dtb.vdt2.vdt1.var_g != 31 || 
                              dtc.vdt2.vdt1.var_g != 31 ) exit(161);
   if ( dtb.vdt2.vdt1.var_h != 35 || 
                              dtc.vdt2.vdt1.var_h != 35 ) exit(163);

/* Test 4 */

   dta = dt0;
   dtp = malloc(sizeof(DT3));
   dtp = st3sum(&dta,&dta);

   dtp = fun4(dtp,&dtb);

   if ( dtb.var_a != 5 || dtp->var_a != 5 ) exit(165);
   if ( dtb.var_b != 9 || dtp->var_b != 9 ) exit(167);
   if ( dtb.var_c != 13 || dtp->var_c != 13 ) exit(169);
   if ( dtb.var_d != 17 || dtp->var_d != 17 ) exit(171);
   if ( dtb.var_e != 21 || dtp->var_e != 21 ) exit(173);
   if ( dtb.var_f != 25 || dtp->var_f != 25 ) exit(175);
   if ( dtb.var_g != 29 || dtp->var_g != 29 ) exit(177);
   if ( dtb.var_h != 33 || dtp->var_h != 33 ) exit(179);

   if ( dtb.vdt2.var_a != 6 || dtp->vdt2.var_a != 6 ) exit(181);
   if ( dtb.vdt2.var_b != 10 || dtp->vdt2.var_b != 10 ) exit(183);
   if ( dtb.vdt2.var_c != 14 || dtp->vdt2.var_c != 14 ) exit(185);
   if ( dtb.vdt2.var_d != 18 || dtp->vdt2.var_d != 18 ) exit(187);
   if ( dtb.vdt2.var_e != 22 || dtp->vdt2.var_e != 22 ) exit(189);
   if ( dtb.vdt2.var_f != 26 || dtp->vdt2.var_f != 26 ) exit(191);
   if ( dtb.vdt2.var_g != 30 || dtp->vdt2.var_g != 30 ) exit(193);
   if ( dtb.vdt2.var_h != 34 || dtp->vdt2.var_h != 34 ) exit(195);

   if ( dtb.vdt2.vdt1.var_a != 7 || 
                              dtp->vdt2.vdt1.var_a != 7 ) exit(197);
   if ( dtb.vdt2.vdt1.var_b != 11 || 
                              dtp->vdt2.vdt1.var_b != 11 ) exit(199);
   if ( dtb.vdt2.vdt1.var_c != 15 || 
                              dtp->vdt2.vdt1.var_c != 15 ) exit(201);
   if ( dtb.vdt2.vdt1.var_d != 19 || 
                              dtp->vdt2.vdt1.var_d != 19 ) exit(203);
   if ( dtb.vdt2.vdt1.var_e != 23 || 
                              dtp->vdt2.vdt1.var_e != 23 ) exit(205);
   if ( dtb.vdt2.vdt1.var_f != 27 || 
                              dtp->vdt2.vdt1.var_f != 27 ) exit(207);
   if ( dtb.vdt2.vdt1.var_g != 31 || 
                              dtp->vdt2.vdt1.var_g != 31 ) exit(209);
   if ( dtb.vdt2.vdt1.var_h != 35 || 
                              dtp->vdt2.vdt1.var_h != 35 ) exit(211);

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
