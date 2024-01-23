
#include <stdio.h>
#include <stdlib.h>


#if ( defined(_AIX) && ! defined(_AIX52) )
  #define int_fast16_t short
#endif

typedef struct dt1 DT1;
typedef struct dt2 DT2;

struct dt1 {
   _Bool var_a;
   double var_b;
   char var_c;
   float var_d;
   long double var_e;
   double var_f;
   _Bool var_g;
   float var_h;
};

struct dt2 {
   float var_a;
   _Bool var_b;
   long double var_c;
   double var_d;
   float var_e;
   _Bool var_f;
   double var_g;
   char var_h;
   DT1 vdt1;
};

DT2 fun1(DT2 *dt) {

   if ( dt->var_a != 2.0f || dt->vdt1.var_a != 1 ) exit(21);
   if ( dt->var_b != 1 || dt->vdt1.var_b != 4 ) exit(23);
   if ( dt->var_c != 6.0l || dt->vdt1.var_c != 'A' ) exit(25);
   if ( dt->var_d != 8.0 || dt->vdt1.var_d != 8 ) exit(27);
   if ( dt->var_e != 10.0f || dt->vdt1.var_e != 10 ) exit(29);
   if ( dt->var_f != 1 || dt->vdt1.var_f != 12 ) exit(31);
   if ( dt->var_g != 14.0 || dt->vdt1.var_g != 1 ) exit(33);
   if ( dt->var_h != 'A' || dt->vdt1.var_h != 16 ) exit(35);

   dt->var_a += 1.0f;
   dt->var_b = 0;
   dt->var_c += 1.0l;
   dt->var_d += 1.0;
   dt->var_e += 1.0f;
   dt->var_f = 0;
   dt->var_g += 1.0;
   dt->var_h = 'B';

   dt->vdt1.var_a = 0;
   dt->vdt1.var_b += 2;
   dt->vdt1.var_c = 'B';
   dt->vdt1.var_d += 2;
   dt->vdt1.var_e += 2;
   dt->vdt1.var_f += 2;
   dt->vdt1.var_g = 0;
   dt->vdt1.var_h += 2;

   return(*dt);
}
