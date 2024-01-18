
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

void sub1(DT2 *dt) {

   if ( dt->var_a != createcomplexf(2.0f,2.0f) || dt->vdt1.var_a != 2 ) exit(21);
   if ( dt->var_b != 4 || dt->vdt1.var_b != createcomplex(4.0,4.0) ) exit(23);
   if ( dt->var_c != createcomplexl(6.0l,6.0l) || dt->vdt1.var_c != 6 ) exit(25);
   if ( dt->var_d != createcomplex(8.0,8.0) || dt->vdt1.var_d != createcomplexf(8.0f,8.0f) ) exit(27);
   if ( dt->var_e != createcomplexf(10.0f,10.0f) || dt->vdt1.var_e != createcomplexl(10.0l,10.0l) ) exit(29);
   if ( dt->var_f != 12 || dt->vdt1.var_f != createcomplex(12.0,12.0) ) exit(31);
   if ( dt->var_g != createcomplex(14.0,14.0) || dt->vdt1.var_g != 14 ) exit(33);
   if ( dt->var_h != 16 || dt->vdt1.var_h != createcomplexf(16.0f,16.0f) ) exit(35);

   dt->var_a += createcomplexf(1.0f,1.0f);
   dt->var_b += 1;
   dt->var_c += createcomplexl(1.0l,1.0l);
   dt->var_d += createcomplex(1.0,1.0);
   dt->var_e += createcomplexf(1.0f,1.0f);
   dt->var_f += 1;
   dt->var_g += createcomplex(1.0,1.0);
   dt->var_h += 1;

   dt->vdt1.var_a += 2;
   dt->vdt1.var_b += createcomplex(2.0,2.0);
   dt->vdt1.var_c += 2;
   dt->vdt1.var_d += createcomplexf(2.0f,2.0f);
   dt->vdt1.var_e += createcomplexl(2.0l,2.0l);
   dt->vdt1.var_f += createcomplex(2.0,2.0);
   dt->vdt1.var_g += 2;
   dt->vdt1.var_h += createcomplexf(2.0f,2.0f);

}

void sub2(DT2 dt) {

   if ( dt.var_a != createcomplexf(2.0f,2.0f) || dt.vdt1.var_a != 2 ) exit(37);
   if ( dt.var_b != 4 || dt.vdt1.var_b != createcomplex(4.0,4.0) ) exit(39);
   if ( dt.var_c != createcomplexl(6.0l,6.0l) || dt.vdt1.var_c != 6 ) exit(41);
   if ( dt.var_d != createcomplex(8.0,8.0) || dt.vdt1.var_d != createcomplexf(8.0f,8.0f) ) exit(43);
   if ( dt.var_e != createcomplexf(10.0f,10.0f) || dt.vdt1.var_e != createcomplexl(10.0l,10.0l) ) exit(45);
   if ( dt.var_f != 12 || dt.vdt1.var_f != createcomplex(12.0,12.0) ) exit(47);
   if ( dt.var_g != createcomplex(14.0,14.0) || dt.vdt1.var_g != 14 ) exit(49);
   if ( dt.var_h != 16 || dt.vdt1.var_h != createcomplexf(16.0f,16.0f) ) exit(51);

   dt.var_a += createcomplexf(1.0f,1.0f);
   dt.var_b += 1;
   dt.var_c += createcomplexl(1.0l,1.0l);
   dt.var_d += createcomplex(1.0,1.0);
   dt.var_e += createcomplexf(1.0f,1.0f);
   dt.var_f += 1;
   dt.var_g += createcomplex(1.0,1.0);
   dt.var_h += 1;

   dt.vdt1.var_a += 2;
   dt.vdt1.var_b += createcomplex(2.0,2.0);
   dt.vdt1.var_c += 2;
   dt.vdt1.var_d += createcomplexf(2.0f,2.0f);
   dt.vdt1.var_e += createcomplexl(2.0l,2.0l);
   dt.vdt1.var_f += createcomplex(2.0,2.0);
   dt.vdt1.var_g += 2;
   dt.vdt1.var_h += createcomplexf(2.0f,2.0f);

}

void sub3(DT2 *dtx, DT2 *dty) {

   if ( dtx->var_a != createcomplexf(4.0f,4.0f) || dtx->vdt1.var_a != 4 ) exit(53);
   if ( dtx->var_b != 8 || dtx->vdt1.var_b != createcomplex(8.0,8.0) ) exit(55);
   if ( dtx->var_c != createcomplexl(12.0l,12.0l) || dtx->vdt1.var_c != 12 ) exit(57);
   if ( dtx->var_d != createcomplex(16.0,16.0) || dtx->vdt1.var_d != createcomplexf(16.0f,16.0f) ) exit(59);
   if ( dtx->var_e != createcomplexf(20.0f,20.0f) || dtx->vdt1.var_e != createcomplexl(20.0l,20.0l) ) exit(61);
   if ( dtx->var_f != 24 || dtx->vdt1.var_f != createcomplex(24.0,24.0) ) exit(63);
   if ( dtx->var_g != createcomplex(28.0,28.0) || dtx->vdt1.var_g != 28 ) exit(65);
   if ( dtx->var_h != 32 || dtx->vdt1.var_h != createcomplexf(32.0f,32.0f) ) exit(67);

   dty->var_a = dtx->var_a + createcomplexf(1.0f,1.0f);
   dty->var_b = dtx->var_b + 1;
   dty->var_c = dtx->var_c + createcomplexl(1.0l,1.0l);
   dty->var_d = dtx->var_d + createcomplex(1.0,1.0);
   dty->var_e = dtx->var_e + createcomplexf(1.0f,1.0f);
   dty->var_f = dtx->var_f + 1;
   dty->var_g = dtx->var_g + createcomplex(1.0,1.0);
   dty->var_h = dtx->var_h + 1;

   dty->vdt1.var_a = dtx->vdt1.var_a + 2;
   dty->vdt1.var_b = dtx->vdt1.var_b + createcomplex(2.0,2.0);
   dty->vdt1.var_c = dtx->vdt1.var_c + 2;
   dty->vdt1.var_d = dtx->vdt1.var_d + createcomplexf(2.0f,2.0f);
   dty->vdt1.var_e = dtx->vdt1.var_e + createcomplexl(2.0l,2.0l);
   dty->vdt1.var_f = dtx->vdt1.var_f + createcomplex(2.0,2.0);
   dty->vdt1.var_g = dtx->vdt1.var_g + 2;
   dty->vdt1.var_h = dtx->vdt1.var_h + createcomplexf(2.0f,2.0f);

}

