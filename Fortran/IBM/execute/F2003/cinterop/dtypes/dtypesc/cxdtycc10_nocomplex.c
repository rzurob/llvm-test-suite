
#include <stdio.h>
#include <stdlib.h>

   #include "cmplx.h"

#include <inttypes.h>
#include <stddef.h>

  #define int_fast16_t short

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

   void sub1(DT2 *dt);
   void sub2(DT2 dt);
   void sub3(DT2 *dtx, DT2 *dty);

   DT2 dt0 = {createcomplexf(2.0f,2.0f),4,createcomplexl(6.0l,6.0l),createcomplex(8.0,8.0),createcomplexf(10.0f,10.0f),12,createcomplex(14.0,14.0),16,
             {2,createcomplex(4.0,4.0),6,createcomplexf(8.0f,8.0f),createcomplexl(10.0l,10.0l),createcomplex(12.0,12.0),14,createcomplexf(16.0f,16.0f)}};

   DT2 dta, dtb, *dtp;

/* Test 1 */

   dta = dt0;

   sub1(&dta);

   if ( dta.var_a != createcomplexf(3.0f,3.0f) || dta.vdt1.var_a != 4 ) exit(21);
   if ( dta.var_b != 5 || dta.vdt1.var_b != createcomplex(6.0,6.0) ) exit(23);
   if ( dta.var_c != createcomplexl(7.0l,7.0l) || dta.vdt1.var_c != 8 ) exit(25);
   if ( dta.var_d != createcomplex(9.0,9.0) || dta.vdt1.var_d != createcomplexf(10.0f,10.0f) ) exit(27);
   if ( dta.var_e != createcomplexf(11.0f,11.0f) || dta.vdt1.var_e != createcomplexl(12.0l,12.0l) ) exit(29);
   if ( dta.var_f != 13 || dta.vdt1.var_f != createcomplex(14.0,14.0) ) exit(31);
   if ( dta.var_g != createcomplex(15.0,15.0) || dta.vdt1.var_g != 16 ) exit(33);
   if ( dta.var_h != 17 || dta.vdt1.var_h != createcomplexf(18.0f,18.0f) ) exit(35);

/* Test 2 */

   dta = dt0;

   sub2(dta);

   if ( dta.var_a != createcomplexf(2.0f,2.0f) || dta.vdt1.var_a != 2 ) exit(37);
   if ( dta.var_b != 4 || dta.vdt1.var_b != createcomplex(4.0,4.0) ) exit(39);
   if ( dta.var_c != createcomplexl(6.0l,6.0l) || dta.vdt1.var_c != 6 ) exit(41);
   if ( dta.var_d != createcomplex(8.0,8.0) || dta.vdt1.var_d != createcomplexf(8.0f,8.0f) ) exit(43);
   if ( dta.var_e != createcomplexf(10.0f,10.0f) || dta.vdt1.var_e != createcomplexl(10.0l,10.0l) ) exit(45);
   if ( dta.var_f != 12 || dta.vdt1.var_f != createcomplex(12.0,12.0) ) exit(47);
   if ( dta.var_g != createcomplex(14.0,14.0) || dta.vdt1.var_g != 14 ) exit(49);
   if ( dta.var_h != 16 || dta.vdt1.var_h != createcomplexf(16.0f,16.0f) ) exit(51);

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT2));
   dtp = st2sum(&dta,&dta);

   sub3(dtp,&dtb);

   if ( dtb.var_a != createcomplexf(5.0f,5.0f) || dtb.vdt1.var_a != 6 ) exit(53);
   if ( dtb.var_b != 9 || dtb.vdt1.var_b != createcomplex(10.0,10.0) ) exit(55);
   if ( dtb.var_c != createcomplexl(13.0l,13.0l) || dtb.vdt1.var_c != 14 ) exit(57);
   if ( dtb.var_d != createcomplex(17.0,17.0) || dtb.vdt1.var_d != createcomplexf(18.0f,18.0f) ) exit(59);
   if ( dtb.var_e != createcomplexf(21.0f,21.0f) || dtb.vdt1.var_e != createcomplexl(22.0l,22.0l) ) exit(61);
   if ( dtb.var_f != 25 || dtb.vdt1.var_f != createcomplex(26.0,26.0) ) exit(63);
   if ( dtb.var_g != createcomplex(29.0,29.0) || dtb.vdt1.var_g != 30 ) exit(65);
   if ( dtb.var_h != 33 || dtb.vdt1.var_h != createcomplexf(34.0f,34.0f) ) exit(67);

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
