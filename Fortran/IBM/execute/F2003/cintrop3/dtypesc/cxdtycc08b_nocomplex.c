
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
