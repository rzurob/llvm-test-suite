
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

DT1 *st1sum(DT1 *dtx, DT1 *dty);
DT2 *st2sum(DT2 *dtx, DT2 *dty);

int main() {

   void sub1(DT2 *dt);
   void sub2(DT2 dt);
   void sub3(DT2 *dtx, DT2 *dty);

   DT2 dt0 = {2,4,6,8,10,12,14,16,
             {2,4,6,8,10,12,14,16}};

   DT2 dta, dtb, *dtp;

/* Test 1 */

   dta = dt0;

   sub1(&dta);

   if ( dta.var_a != 3 || dta.vdt1.var_a != 4 ) exit(21);
   if ( dta.var_b != 5 || dta.vdt1.var_b != 6 ) exit(23);
   if ( dta.var_c != 7 || dta.vdt1.var_c != 8 ) exit(25);
   if ( dta.var_d != 9 || dta.vdt1.var_d != 10 ) exit(27);
   if ( dta.var_e != 11 || dta.vdt1.var_e != 12 ) exit(29);
   if ( dta.var_f != 13 || dta.vdt1.var_f != 14 ) exit(31);
   if ( dta.var_g != 15 || dta.vdt1.var_g != 16 ) exit(33);
   if ( dta.var_h != 17 || dta.vdt1.var_h != 18 ) exit(35);

/* Test 2 */

   dta = dt0;

   sub2(dta);

   if ( dta.var_a != 2 || dta.vdt1.var_a != 2 ) exit(37);
   if ( dta.var_b != 4 || dta.vdt1.var_b != 4 ) exit(39);
   if ( dta.var_c != 6 || dta.vdt1.var_c != 6 ) exit(41);
   if ( dta.var_d != 8 || dta.vdt1.var_d != 8 ) exit(43);
   if ( dta.var_e != 10 || dta.vdt1.var_e != 10 ) exit(45);
   if ( dta.var_f != 12 || dta.vdt1.var_f != 12 ) exit(47);
   if ( dta.var_g != 14 || dta.vdt1.var_g != 14 ) exit(49);
   if ( dta.var_h != 16 || dta.vdt1.var_h != 16 ) exit(51);

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT2));
   dtp = st2sum(&dta,&dta);

   sub3(dtp,&dtb);

   if ( dtb.var_a != 5 || dtb.vdt1.var_a != 6 ) exit(53);
   if ( dtb.var_b != 9 || dtb.vdt1.var_b != 10 ) exit(55);
   if ( dtb.var_c != 13 || dtb.vdt1.var_c != 14 ) exit(57);
   if ( dtb.var_d != 17 || dtb.vdt1.var_d != 18 ) exit(59);
   if ( dtb.var_e != 21 || dtb.vdt1.var_e != 22 ) exit(61);
   if ( dtb.var_f != 25 || dtb.vdt1.var_f != 26 ) exit(63);
   if ( dtb.var_g != 29 || dtb.vdt1.var_g != 30 ) exit(65);
   if ( dtb.var_h != 33 || dtb.vdt1.var_h != 34 ) exit(67);

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
