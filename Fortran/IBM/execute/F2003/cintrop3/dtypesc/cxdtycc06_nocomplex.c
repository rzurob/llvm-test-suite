
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

   DT3 dt0 = {createcomplexf(2.0f,2.0f),4,createcomplexl(6.0l,6.0l),createcomplex(8.0,8.0),createcomplexf(10.0f,10.0f),12,createcomplex(14.0,14.0),16,
             {{2,createcomplex(4.0,4.0),6,createcomplexf(8.0f,8.0f),createcomplexl(10.0l,10.0l),createcomplex(12.0,12.0),14,createcomplexf(16.0f,16.0f)},
             createcomplex(2.0,2.0),4,createcomplex(6.0,6.0),8,createcomplexl(10.0l,10.0l),createcomplexf(12.0f,12.0f),14,createcomplexf(16.0f,16.0f)}};

   DT3 dta, dtb, *dtp;

/* Test 1 */

   dta = dt0;

   sub1(&dta);

   if ( dta.var_a != createcomplexf(3.0f,3.0f) || dta.vdt2.var_a != createcomplex(4.0,4.0) ||
                          dta.vdt2.vdt1.var_a != 5 ) exit(21);

   if ( dta.var_b != 5 || dta.vdt2.var_b != 6 ||
                          dta.vdt2.vdt1.var_b != createcomplex(7.0,7.0) ) exit(23);

   if ( dta.var_c != createcomplexl(7.0l,7.0l) || dta.vdt2.var_c != createcomplex(8.0,8.0) ||
                          dta.vdt2.vdt1.var_c != 9 ) exit(25);

   if ( dta.var_d != createcomplex(9.0,9.0) || dta.vdt2.var_d != 10 ||
                          dta.vdt2.vdt1.var_d != createcomplexf(11.0f,11.0f) ) exit(27);

   if ( dta.var_e != createcomplexf(11.0f,11.0f) || dta.vdt2.var_e != createcomplexl(12.0l,12.0l) ||
                           dta.vdt2.vdt1.var_e != createcomplexl(13.0l,13.0l) ) exit(29);

   if ( dta.var_f != 13 || dta.vdt2.var_f != createcomplexf(14.0f,14.0f) ||
                           dta.vdt2.vdt1.var_f != createcomplex(15.0,15.0) ) exit(31);

   if ( dta.var_g != createcomplex(15.0,15.0) || dta.vdt2.var_g != 16 ||
                           dta.vdt2.vdt1.var_g != 17 ) exit(33);

   if ( dta.var_h != 17 || dta.vdt2.var_h != createcomplexf(18.0f,18.0f) ||
                           dta.vdt2.vdt1.var_h != createcomplexf(19.0f,19.0f) ) exit(35);

/* Test 2 */

   dta = dt0;

   sub2(dta);

   if ( dta.var_a != createcomplexf(2.0f,2.0f) || dta.vdt2.var_a != createcomplex(2.0,2.0) ||
                          dta.vdt2.vdt1.var_a != 2 ) exit(37);

   if ( dta.var_b != 4 || dta.vdt2.var_b != 4 ||
                          dta.vdt2.vdt1.var_b != createcomplex(4.0,4.0) ) exit(39);

   if ( dta.var_c != createcomplexl(6.0l,6.0l) || dta.vdt2.var_c != createcomplex(6.0,6.0) ||
                          dta.vdt2.vdt1.var_c != 6 ) exit(41);

   if ( dta.var_d != createcomplex(8.0,8.0) || dta.vdt2.var_d != 8 ||
                          dta.vdt2.vdt1.var_d != createcomplexf(8.0f,8.0f) ) exit(43);

   if ( dta.var_e != createcomplexf(10.0f,10.0f) || dta.vdt2.var_e != createcomplexl(10.0l,10.0l) ||
                           dta.vdt2.vdt1.var_e != createcomplexl(10.0l,10.0l) ) exit(45);

   if ( dta.var_f != 12 || dta.vdt2.var_f != createcomplexf(12.0f,12.0f) ||
                           dta.vdt2.vdt1.var_f != createcomplex(12.0,12.0) ) exit(47);

   if ( dta.var_g != createcomplex(14.0,14.0) || dta.vdt2.var_g != 14 ||
                           dta.vdt2.vdt1.var_g != 14 ) exit(49);

   if ( dta.var_h != 16 || dta.vdt2.var_h != createcomplexf(16.0f,16.0f) ||
                           dta.vdt2.vdt1.var_h != createcomplexf(16.0f,16.0f) ) exit(51);

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT3));
   dtp = st3sum(&dta,&dta);

   sub3(dtp,&dtb);

   if ( dtb.var_a != createcomplexf(5.0f,5.0f) || dtb.vdt2.var_a != createcomplex(6.0,6.0) ||
                          dtb.vdt2.vdt1.var_a != 7 ) exit(53);

   if ( dtb.var_b != 9 || dtb.vdt2.var_b != 10 ||
                          dtb.vdt2.vdt1.var_b != createcomplex(11.0,11.0) ) exit(55);

   if ( dtb.var_c != createcomplexl(13.0l,13.0l) || dtb.vdt2.var_c != createcomplex(14.0,14.0) ||
                           dtb.vdt2.vdt1.var_c != 15 ) exit(57);

   if ( dtb.var_d != createcomplex(17.0,17.0) || dtb.vdt2.var_d != 18 ||
                           dtb.vdt2.vdt1.var_d != createcomplexf(19.0f,19.0f) ) exit(59);

   if ( dtb.var_e != createcomplexf(21.0f,21.0f) || dtb.vdt2.var_e != createcomplexl(22.0l,22.0l) ||
                           dtb.vdt2.vdt1.var_e != createcomplexl(23.0l,23.0l) ) exit(61);

   if ( dtb.var_f != 25 || dtb.vdt2.var_f != createcomplexf(26.0f,26.0f) ||
                           dtb.vdt2.vdt1.var_f != createcomplex(27.0,27.0) ) exit(63);

   if ( dtb.var_g != createcomplex(29.0,29.0) || dtb.vdt2.var_g != 30 ||
                           dtb.vdt2.vdt1.var_g != 31 ) exit(65);

   if ( dtb.var_h != 33 || dtb.vdt2.var_h != createcomplexf(34.0f,34.0f) ||
                           dtb.vdt2.vdt1.var_h != createcomplexf(35.0f,35.0f) ) exit(67);

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
