
#include <stdio.h>
#include <stdlib.h>

   #include <inttypes.h>
   #include <stddef.h>


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

DT2 fun1(DT2 *dt) {

   if ( dt->var_a != 2 || dt->vdt1.var_a != 2 ) exit(21);
   if ( dt->var_b != 4 || dt->vdt1.var_b != 4 ) exit(23);
   if ( dt->var_c != 6 || dt->vdt1.var_c != 6 ) exit(25);
   if ( dt->var_d != 8 || dt->vdt1.var_d != 8 ) exit(27);
   if ( dt->var_e != 10 || dt->vdt1.var_e != 10 ) exit(29);
   if ( dt->var_f != 12 || dt->vdt1.var_f != 12 ) exit(31);
   if ( dt->var_g != 14 || dt->vdt1.var_g != 14 ) exit(33);
   if ( dt->var_h != 16 || dt->vdt1.var_h != 16 ) exit(35);

   dt->var_a += 1;
   dt->var_b += 1;
   dt->var_c += 1;
   dt->var_d += 1;
   dt->var_e += 1;
   dt->var_f += 1;
   dt->var_g += 1;
   dt->var_h += 1;

   dt->vdt1.var_a += 2;
   dt->vdt1.var_b += 2;
   dt->vdt1.var_c += 2;
   dt->vdt1.var_d += 2;
   dt->vdt1.var_e += 2;
   dt->vdt1.var_f += 2;
   dt->vdt1.var_g += 2;
   dt->vdt1.var_h += 2;

   return(*dt);
}
