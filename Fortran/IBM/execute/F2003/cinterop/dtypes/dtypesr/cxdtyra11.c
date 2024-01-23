
#include <stdio.h>
#include <stdlib.h>

#ifdef STDINT
   #include <stdint.h>
   #include <stddef.h>
#endif

#if ( defined(_AIX) && ! defined(_AIX52) )
  #define int_fast16_t short
#endif

typedef struct dt1 DT1;
typedef struct dt2 DT2;

struct dt1 {
   int_fast16_t var_a;
   double var_b;
   signed char var_c;
   float var_d;
   long double var_e;
   double var_f;
   intmax_t var_g;
   float var_h;
};

struct dt2 {
   float var_a;
   short var_b;
   long double var_c;
   double var_d;
   float var_e;
   char var_f;
   double var_g;
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

   DT2 dt0 = {2.0f,4,6.0l,8.0,10.0f,12,14.0,16,
             {2,4.0,6,8.0f,10.0l,12.0,14,16.0f}};

   DT2 dta, dtb, dtc, *dtp;

/* Test 1 */

   dta = dt0;

   dtb = fun1(&dta);

   if ( dta.var_a != 3.0f || dtb.var_a != 3.0f ) exit(21);
   if ( dta.var_b != 5 || dtb.var_b != 5 ) exit(23);
   if ( dta.var_c != 7.0l || dtb.var_c != 7.0l ) exit(25);
   if ( dta.var_d != 9.0 || dtb.var_d != 9.0 ) exit(27);
   if ( dta.var_e != 11.0f || dtb.var_e != 11.0f ) exit(29);
   if ( dta.var_f != 13 || dtb.var_f != 13 ) exit(31);
   if ( dta.var_g != 15.0 || dtb.var_g != 15.0 ) exit(33);
   if ( dta.var_h != 17 || dtb.var_h != 17 ) exit(35);

   if ( dta.vdt1.var_a != 4 || dtb.vdt1.var_a != 4 ) exit(37);
   if ( dta.vdt1.var_b != 6.0 || dtb.vdt1.var_b != 6.0 ) exit(39);
   if ( dta.vdt1.var_c != 8 || dtb.vdt1.var_c != 8 ) exit(41);
   if ( dta.vdt1.var_d != 10.0f || dtb.vdt1.var_d != 10.0f ) exit(43);
   if ( dta.vdt1.var_e != 12.0l || dtb.vdt1.var_e != 12.0l ) exit(45);
   if ( dta.vdt1.var_f != 14.0 || dtb.vdt1.var_f != 14.0 ) exit(47);
   if ( dta.vdt1.var_g != 16 || dtb.vdt1.var_g != 16 ) exit(49);
   if ( dta.vdt1.var_h != 18.0f || dtb.vdt1.var_h != 18.0f ) exit(51);

/* Test 2 */

   dta = dt0;

   dtb = fun2(dta);

   if ( dta.var_a != 2.0f || dtb.var_a != 3.0f ) exit(53);
   if ( dta.var_b != 4 || dtb.var_b != 5 ) exit(55);
   if ( dta.var_c != 6.0l || dtb.var_c != 7.0l ) exit(57);
   if ( dta.var_d != 8.0 || dtb.var_d != 9.0 ) exit(59);
   if ( dta.var_e != 10.0f || dtb.var_e != 11.0f ) exit(61);
   if ( dta.var_f != 12 || dtb.var_f != 13 ) exit(63);
   if ( dta.var_g != 14.0 || dtb.var_g != 15.0 ) exit(65);
   if ( dta.var_h != 16 || dtb.var_h != 17 ) exit(67);

   if ( dta.vdt1.var_a != 2 || dtb.vdt1.var_a != 4 ) exit(69);
   if ( dta.vdt1.var_b != 4.0 || dtb.vdt1.var_b != 6.0 ) exit(71);
   if ( dta.vdt1.var_c != 6 || dtb.vdt1.var_c != 8 ) exit(73);
   if ( dta.vdt1.var_d != 8.0f || dtb.vdt1.var_d != 10.0f ) exit(75);
   if ( dta.vdt1.var_e != 10.0l || dtb.vdt1.var_e != 12.0l ) exit(77);
   if ( dta.vdt1.var_f != 12.0 || dtb.vdt1.var_f != 14.0 ) exit(79);
   if ( dta.vdt1.var_g != 14 || dtb.vdt1.var_g != 16 ) exit(81);
   if ( dta.vdt1.var_h != 16.0f || dtb.vdt1.var_h != 18.0f ) exit(83);

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT2));
   dtp = st2sum(&dta,&dta);

   dtc = fun3(dtp,&dtb);

   if ( dtb.var_a != 5.0f || dtc.var_a != 5.0f ) exit(85);
   if ( dtb.var_b != 9 || dtc.var_b != 9 ) exit(87);
   if ( dtb.var_c != 13.0l || dtc.var_c != 13.0l ) exit(89);
   if ( dtb.var_d != 17.0 || dtc.var_d != 17.0 ) exit(91);
   if ( dtb.var_e != 21.0f || dtc.var_e != 21.0f ) exit(93);
   if ( dtb.var_f != 25 || dtc.var_f != 25 ) exit(95);
   if ( dtb.var_g != 29.0 || dtc.var_g != 29.0 ) exit(97);
   if ( dtb.var_h != 33 || dtc.var_h != 33 ) exit(99);

   if ( dtb.vdt1.var_a != 6 || dtc.vdt1.var_a != 6 ) exit(101);
   if ( dtb.vdt1.var_b != 10.0 || dtc.vdt1.var_b != 10.0 ) exit(103);
   if ( dtb.vdt1.var_c != 14 || dtc.vdt1.var_c != 14 ) exit(105);
   if ( dtb.vdt1.var_d != 18.0f || dtc.vdt1.var_d != 18.0f ) exit(107);
   if ( dtb.vdt1.var_e != 22.0l || dtc.vdt1.var_e != 22.0l ) exit(109);
   if ( dtb.vdt1.var_f != 26.0 || dtc.vdt1.var_f != 26.0 ) exit(111);
   if ( dtb.vdt1.var_g != 30 || dtc.vdt1.var_g != 30 ) exit(113);
   if ( dtb.vdt1.var_h != 34.0f || dtc.vdt1.var_h != 34.0f ) exit(115);

/* Test 4 */

   dta = dt0;
   dtp = malloc(sizeof(DT2));
   dtp = st2sum(&dta,&dta);

   dtp = fun4(dtp,&dtb);

   if ( dtb.var_a != 5.0f || dtp->var_a != 5.0f ) exit(117);
   if ( dtb.var_b != 9 || dtp->var_b != 9 ) exit(119);
   if ( dtb.var_c != 13.0l || dtp->var_c != 13.0l ) exit(121);
   if ( dtb.var_d != 17.0 || dtp->var_d != 17.0 ) exit(123);
   if ( dtb.var_e != 21.0f || dtp->var_e != 21.0f ) exit(125);
   if ( dtb.var_f != 25 || dtp->var_f != 25 ) exit(127);
   if ( dtb.var_g != 29.0 || dtp->var_g != 29.0 ) exit(129);
   if ( dtb.var_h != 33 || dtp->var_h != 33 ) exit(131);

   if ( dtb.vdt1.var_a != 6 || dtp->vdt1.var_a != 6 ) exit(133);
   if ( dtb.vdt1.var_b != 10.0 || dtp->vdt1.var_b != 10.0 ) exit(135);
   if ( dtb.vdt1.var_c != 14 || dtp->vdt1.var_c != 14 ) exit(137);
   if ( dtb.vdt1.var_d != 18.0f || dtp->vdt1.var_d != 18.0f ) exit(139);
   if ( dtb.vdt1.var_e != 22.0l || dtp->vdt1.var_e != 22.0l ) exit(141);
   if ( dtb.vdt1.var_f != 26.0 || dtp->vdt1.var_f != 26.0 ) exit(143);
   if ( dtb.vdt1.var_g != 30 || dtp->vdt1.var_g != 30 ) exit(145);
   if ( dtb.vdt1.var_h != 34.0f || dtp->vdt1.var_h != 34.0f ) exit(147);

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
