
#include <stdio.h>
#include <stdlib.h>

   #include "cmplx.h"

typedef struct dt1 DT1;

struct dt1 {
   float _Complex var_a;
   short var_b;
   long double _Complex var_c;
   double _Complex var_d;
   float _Complex var_e;
   char var_f;
   double _Complex var_g;
   int var_h;
};

void sub1(DT1 *dt) {

   if ( dt->var_a != createcomplexf(2.0f,2.0f) ) exit(21);
   if ( dt->var_b != 4 ) exit(23);
   if ( dt->var_c != createcomplexl(6.0l,6.0l) ) exit(25);
   if ( dt->var_d != createcomplex(8.0,8.0) ) exit(27);
   if ( dt->var_e != createcomplexf(10.0f,10.0f) ) exit(29);
   if ( dt->var_f != 12 ) exit(31);
   if ( dt->var_g != createcomplex(14.0,14.0) ) exit(33);
   if ( dt->var_h != 16 ) exit(35);

   dt->var_a += createcomplexf(1.0f,1.0f);
   dt->var_b += 1;
   dt->var_c += createcomplexl(1.0l,1.0l);
   dt->var_d += createcomplex(1.0,1.0);
   dt->var_e += createcomplexf(1.0f,1.0f);
   dt->var_f += 1;
   dt->var_g += createcomplex(1.0,1.0);
   dt->var_h += 1;
}

void sub2(DT1 dt) {

   if ( dt.var_a != createcomplexf(2.0f,2.0f) ) exit(37);
   if ( dt.var_b != 4 ) exit(39);
   if ( dt.var_c != createcomplexl(6.0l,6.0l) ) exit(41);
   if ( dt.var_d != createcomplex(8.0,8.0) ) exit(43);
   if ( dt.var_e != createcomplexf(10.0f,10.0f) ) exit(45);
   if ( dt.var_f != 12 ) exit(47);
   if ( dt.var_g != createcomplex(14.0,14.0) ) exit(49);
   if ( dt.var_h != 16 ) exit(51);

   dt.var_a += createcomplexf(1.0f,1.0f);
   dt.var_b += 1;
   dt.var_c += createcomplexl(1.0l,1.0l);
   dt.var_d += createcomplex(1.0,1.0);
   dt.var_e += createcomplexf(1.0f,1.0f);
   dt.var_f += 1;
   dt.var_g += createcomplex(1.0,1.0);
   dt.var_h += 1;
}

void sub3(DT1 *dtx, DT1 *dty) {

   if ( dtx->var_a != createcomplexf(4.0f,4.0f) ) exit(53);
   if ( dtx->var_b != 8 ) exit(55);
   if ( dtx->var_c != createcomplexl(12.0l,12.0l) ) exit(57);
   if ( dtx->var_d != createcomplex(16.0,16.0) ) exit(59);
   if ( dtx->var_e != createcomplexf(20.0f,20.0f) ) exit(61);
   if ( dtx->var_f != 24 ) exit(63);
   if ( dtx->var_g != createcomplex(28.0,28.0) ) exit(65);
   if ( dtx->var_h != 32 ) exit(67);

   dty->var_a = dtx->var_a + createcomplexf(1.0f,1.0f);
   dty->var_b = dtx->var_b + 1;
   dty->var_c = dtx->var_c + createcomplexl(1.0l,1.0l);
   dty->var_d = dtx->var_d + createcomplex(1.0,1.0);
   dty->var_e = dtx->var_e + createcomplexf(1.0f,1.0f);
   dty->var_f = dtx->var_f + 1;
   dty->var_g = dtx->var_g + createcomplex(1.0,1.0);
   dty->var_h = dtx->var_h + 1;
   
}
