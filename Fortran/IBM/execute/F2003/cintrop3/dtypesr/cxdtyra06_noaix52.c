
#include <stdio.h>
#include <stdlib.h>

   #include <inttypes.h>
   #include <stddef.h>

  #define int_fast16_t short

typedef struct dt1 DT1;
typedef struct dt2 DT2;
typedef struct dt3 DT3;

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
   DT1 vdt1;
   double var_a;
   int var_b;
   double var_c;
   char var_d;
   long double var_e;
   float var_f;
   int32_t var_g;
   float var_h;
};

struct dt3 {
   float var_a;
   short var_b;
   long double var_c;
   double var_d;
   float var_e;
   char var_f;
   double var_g;
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

   DT3 dt0 = {2.0f,4,6.0l,8.0,10.0f,12,14.0,16,
             {{2,4.0,6,8.0f,10.0l,12.0,14,16.0f},
             2.0,4,6.0,8,10.0l,12.0f,14,16.0f}};

   DT3 dta, dtb, *dtp;

/* Test 1 */

   dta = dt0;

   sub1(&dta);

   if ( dta.var_a != 3.0f || dta.vdt2.var_a != 4.0 ||
                          dta.vdt2.vdt1.var_a != 5 ) exit(21);

   if ( dta.var_b != 5 || dta.vdt2.var_b != 6 ||
                          dta.vdt2.vdt1.var_b != 7.0 ) exit(23);

   if ( dta.var_c != 7.0l || dta.vdt2.var_c != 8.0 ||
                          dta.vdt2.vdt1.var_c != 9 ) exit(25);

   if ( dta.var_d != 9.0 || dta.vdt2.var_d != 10 ||
                          dta.vdt2.vdt1.var_d != 11.0f ) exit(27);

   if ( dta.var_e != 11.0f || dta.vdt2.var_e != 12.0l ||
                           dta.vdt2.vdt1.var_e != 13.0l ) exit(29);

   if ( dta.var_f != 13 || dta.vdt2.var_f != 14.0f ||
                           dta.vdt2.vdt1.var_f != 15.0 ) exit(31);

   if ( dta.var_g != 15.0 || dta.vdt2.var_g != 16 ||
                           dta.vdt2.vdt1.var_g != 17 ) exit(33);

   if ( dta.var_h != 17 || dta.vdt2.var_h != 18.0f ||
                           dta.vdt2.vdt1.var_h != 19.0f ) exit(35);

/* Test 2 */

   dta = dt0;

   sub2(dta);

   if ( dta.var_a != 2.0f || dta.vdt2.var_a != 2.0 ||
                          dta.vdt2.vdt1.var_a != 2 ) exit(37);

   if ( dta.var_b != 4 || dta.vdt2.var_b != 4 ||
                          dta.vdt2.vdt1.var_b != 4.0 ) exit(39);

   if ( dta.var_c != 6.0l || dta.vdt2.var_c != 6.0 ||
                          dta.vdt2.vdt1.var_c != 6 ) exit(41);

   if ( dta.var_d != 8.0 || dta.vdt2.var_d != 8 ||
                          dta.vdt2.vdt1.var_d != 8.0f ) exit(43);

   if ( dta.var_e != 10.0f || dta.vdt2.var_e != 10.0l ||
                           dta.vdt2.vdt1.var_e != 10.0l ) exit(45);

   if ( dta.var_f != 12 || dta.vdt2.var_f != 12.0f ||
                           dta.vdt2.vdt1.var_f != 12.0 ) exit(47);

   if ( dta.var_g != 14.0 || dta.vdt2.var_g != 14 ||
                           dta.vdt2.vdt1.var_g != 14 ) exit(49);

   if ( dta.var_h != 16 || dta.vdt2.var_h != 16.0f ||
                           dta.vdt2.vdt1.var_h != 16.0f ) exit(51);

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT3));
   dtp = st3sum(&dta,&dta);

   sub3(dtp,&dtb);

   if ( dtb.var_a != 5.0f || dtb.vdt2.var_a != 6.0 ||
                          dtb.vdt2.vdt1.var_a != 7 ) exit(53);

   if ( dtb.var_b != 9 || dtb.vdt2.var_b != 10 ||
                          dtb.vdt2.vdt1.var_b != 11.0 ) exit(55);

   if ( dtb.var_c != 13.0l || dtb.vdt2.var_c != 14.0 ||
                           dtb.vdt2.vdt1.var_c != 15 ) exit(57);

   if ( dtb.var_d != 17.0 || dtb.vdt2.var_d != 18 ||
                           dtb.vdt2.vdt1.var_d != 19.0f ) exit(59);

   if ( dtb.var_e != 21.0f || dtb.vdt2.var_e != 22.0l ||
                           dtb.vdt2.vdt1.var_e != 23.0l ) exit(61);

   if ( dtb.var_f != 25 || dtb.vdt2.var_f != 26.0f ||
                           dtb.vdt2.vdt1.var_f != 27.0 ) exit(63);

   if ( dtb.var_g != 29.0 || dtb.vdt2.var_g != 30 ||
                           dtb.vdt2.vdt1.var_g != 31 ) exit(65);

   if ( dtb.var_h != 33 || dtb.vdt2.var_h != 34.0f ||
                           dtb.vdt2.vdt1.var_h != 35.0f ) exit(67);

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
