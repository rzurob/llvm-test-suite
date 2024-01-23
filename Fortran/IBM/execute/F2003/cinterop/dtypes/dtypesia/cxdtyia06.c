
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
   DT1 vdt1;
   int16_t var_a;
   int_fast8_t var_b;
   int64_t var_c;
   int8_t var_d;
   int_fast32_t var_e;
   int_fast16_t var_f;
   int32_t var_g;
   int_fast64_t var_h;
};

struct dt3 {
   size_t var_a;
   int_least8_t var_b;
   int_least64_t var_c;
   signed char var_d;
   intptr_t var_e;
   int_least16_t var_f;
   intmax_t var_g;
   int_least32_t var_h;
   DT2 vdt2;
};

DT1 *st1sum(DT1 *dtx, DT1 *dty);
DT2 *st2sum(DT2 *dtx, DT2 *dty);
DT3 *st3sum(DT3 *dtx, DT3 *dty);

int main() {

   void sub1(DT3 *dt);
   void sub2(DT3 dt);
   void sub3(DT3 *dtx, DT3 *dty);

   DT3 dt0 = {2,4,6,8,10,12,14,16,
             {{2,4,6,8,10,12,14,16},
             2,4,6,8,10,12,14,16}};

   DT3 dta, dtb, *dtp;

/* Test 1 */

   dta = dt0;

   sub1(&dta);

   if ( dta.var_a != 3 || dta.vdt2.var_a != 4 ||
                          dta.vdt2.vdt1.var_a != 5 ) exit(21);

   if ( dta.var_b != 5 || dta.vdt2.var_b != 6 ||
                          dta.vdt2.vdt1.var_b != 7 ) exit(23);

   if ( dta.var_c != 7 || dta.vdt2.var_c != 8 ||
                          dta.vdt2.vdt1.var_c != 9 ) exit(25);

   if ( dta.var_d != 9 || dta.vdt2.var_d != 10 ||
                          dta.vdt2.vdt1.var_d != 11 ) exit(27);

   if ( dta.var_e != 11 || dta.vdt2.var_e != 12 ||
                           dta.vdt2.vdt1.var_e != 13 ) exit(29);

   if ( dta.var_f != 13 || dta.vdt2.var_f != 14 ||
                           dta.vdt2.vdt1.var_f != 15 ) exit(31);

   if ( dta.var_g != 15 || dta.vdt2.var_g != 16 ||
                           dta.vdt2.vdt1.var_g != 17 ) exit(33);

   if ( dta.var_h != 17 || dta.vdt2.var_h != 18 ||
                           dta.vdt2.vdt1.var_h != 19 ) exit(35);

/* Test 2 */

   dta = dt0;

   sub2(dta);

   if ( dta.var_a != 2 || dta.vdt2.var_a != 2 ||
                          dta.vdt2.vdt1.var_a != 2 ) exit(37);

   if ( dta.var_b != 4 || dta.vdt2.var_b != 4 ||
                          dta.vdt2.vdt1.var_b != 4 ) exit(39);

   if ( dta.var_c != 6 || dta.vdt2.var_c != 6 ||
                          dta.vdt2.vdt1.var_c != 6 ) exit(41);

   if ( dta.var_d != 8 || dta.vdt2.var_d != 8 ||
                          dta.vdt2.vdt1.var_d != 8 ) exit(43);

   if ( dta.var_e != 10 || dta.vdt2.var_e != 10 ||
                           dta.vdt2.vdt1.var_e != 10 ) exit(45);

   if ( dta.var_f != 12 || dta.vdt2.var_f != 12 ||
                           dta.vdt2.vdt1.var_f != 12 ) exit(47);

   if ( dta.var_g != 14 || dta.vdt2.var_g != 14 ||
                           dta.vdt2.vdt1.var_g != 14 ) exit(49);

   if ( dta.var_h != 16 || dta.vdt2.var_h != 16 ||
                           dta.vdt2.vdt1.var_h != 16 ) exit(51);

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT3));
   dtp = st3sum(&dta,&dta);

   sub3(dtp,&dtb);

   if ( dtb.var_a != 5 || dtb.vdt2.var_a != 6 ||
                          dtb.vdt2.vdt1.var_a != 7 ) exit(53);

   if ( dtb.var_b != 9 || dtb.vdt2.var_b != 10 ||
                          dtb.vdt2.vdt1.var_b != 11 ) exit(55);

   if ( dtb.var_c != 13 || dtb.vdt2.var_c != 14 ||
                           dtb.vdt2.vdt1.var_c != 15 ) exit(57);

   if ( dtb.var_d != 17 || dtb.vdt2.var_d != 18 ||
                           dtb.vdt2.vdt1.var_d != 19 ) exit(59);

   if ( dtb.var_e != 21 || dtb.vdt2.var_e != 22 ||
                           dtb.vdt2.vdt1.var_e != 23 ) exit(61);

   if ( dtb.var_f != 25 || dtb.vdt2.var_f != 26 ||
                           dtb.vdt2.vdt1.var_f != 27 ) exit(63);

   if ( dtb.var_g != 29 || dtb.vdt2.var_g != 30 ||
                           dtb.vdt2.vdt1.var_g != 31 ) exit(65);

   if ( dtb.var_h != 33 || dtb.vdt2.var_h != 34 ||
                           dtb.vdt2.vdt1.var_h != 35 ) exit(67);

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
