
#include <stdio.h>
#include <stdlib.h>

   #include <complex.h>

#include <inttypes.h>
#include <stddef.h>

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

   if ( dt->var_a != 2.0f+I*2.0f || dt->vdt1.var_a != 2 ) exit(21);
   if ( dt->var_b != 4 || dt->vdt1.var_b != 4.0+I*4.0 ) exit(23);
   if ( dt->var_c != 6.0l+I*6.0l || dt->vdt1.var_c != 6 ) exit(25);
   if ( dt->var_d != 8.0+I*8.0 || dt->vdt1.var_d != 8.0f+I*8.0f ) exit(27);
   if ( dt->var_e != 10.0f+I*10.0f || dt->vdt1.var_e != 10.0l+I*10.0l ) exit(29);
   if ( dt->var_f != 12 || dt->vdt1.var_f != 12.0+I*12.0 ) exit(31);
   if ( dt->var_g != 14.0+I*14.0 || dt->vdt1.var_g != 14 ) exit(33);
   if ( dt->var_h != 16 || dt->vdt1.var_h != 16.0f+I*16.0f ) exit(35);

   dt->var_a += 1.0f+I*1.0f;
   dt->var_b += 1;
   dt->var_c += 1.0l+I*1.0l;
   dt->var_d += 1.0+I*1.0;
   dt->var_e += 1.0f+I*1.0f;
   dt->var_f += 1;
   dt->var_g += 1.0+I*1.0;
   dt->var_h += 1;

   dt->vdt1.var_a += 2;
   dt->vdt1.var_b += 2.0+I*2.0;
   dt->vdt1.var_c += 2;
   dt->vdt1.var_d += 2.0f+I*2.0f;
   dt->vdt1.var_e += 2.0l+I*2.0l;
   dt->vdt1.var_f += 2.0+I*2.0;
   dt->vdt1.var_g += 2;
   dt->vdt1.var_h += 2.0f+I*2.0f;

}
