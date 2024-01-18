
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
   float var_a;
   short var_b;
   long double var_c;
   double var_d;
   float var_e;
   char var_f;
   double var_g;
   int var_h;
   DT1 vdt1;
};

DT2 fun1(DT2 *dt) {

   if ( dt->var_a != 2.0f || dt->vdt1.var_a != 2 ) exit(21);
   if ( dt->var_b != 4 || dt->vdt1.var_b != 4.0 ) exit(23);
   if ( dt->var_c != 6.0l || dt->vdt1.var_c != 6 ) exit(25);
   if ( dt->var_d != 8.0 || dt->vdt1.var_d != 8.0f ) exit(27);
   if ( dt->var_e != 10.0f || dt->vdt1.var_e != 10.0l ) exit(29);
   if ( dt->var_f != 12 || dt->vdt1.var_f != 12.0 ) exit(31);
   if ( dt->var_g != 14.0 || dt->vdt1.var_g != 14 ) exit(33);
   if ( dt->var_h != 16 || dt->vdt1.var_h != 16.0f ) exit(35);

   dt->var_a += 1.0f;
   dt->var_b += 1;
   dt->var_c += 1.0l;
   dt->var_d += 1.0;
   dt->var_e += 1.0f;
   dt->var_f += 1;
   dt->var_g += 1.0;
   dt->var_h += 1;

   dt->vdt1.var_a += 2;
   dt->vdt1.var_b += 2.0;
   dt->vdt1.var_c += 2;
   dt->vdt1.var_d += 2.0f;
   dt->vdt1.var_e += 2.0l;
   dt->vdt1.var_f += 2.0;
   dt->vdt1.var_g += 2;
   dt->vdt1.var_h += 2.0f;

   return(*dt);
}

DT2 fun2(DT2 dt) {

   DT2 *dtp;

   dtp = malloc(sizeof(DT2));

   if ( dt.var_a != 2.0f || dt.vdt1.var_a != 2 ) exit(37);
   if ( dt.var_b != 4 || dt.vdt1.var_b != 4.0 ) exit(39);
   if ( dt.var_c != 6.0l || dt.vdt1.var_c != 6 ) exit(41);
   if ( dt.var_d != 8.0 || dt.vdt1.var_d != 8.0f ) exit(43);
   if ( dt.var_e != 10.0f || dt.vdt1.var_e != 10.0l ) exit(45);
   if ( dt.var_f != 12 || dt.vdt1.var_f != 12.0 ) exit(47);
   if ( dt.var_g != 14.0 || dt.vdt1.var_g != 14 ) exit(49);
   if ( dt.var_h != 16 || dt.vdt1.var_h != 16.0f ) exit(51);

   dt.var_a += 1.0f;
   dt.var_b += 1;
   dt.var_c += 1.0l;
   dt.var_d += 1.0;
   dt.var_e += 1.0f;
   dt.var_f += 1;
   dt.var_g += 1.0;
   dt.var_h += 1;

   dt.vdt1.var_a += 2;
   dt.vdt1.var_b += 2.0;
   dt.vdt1.var_c += 2;
   dt.vdt1.var_d += 2.0f;
   dt.vdt1.var_e += 2.0l;
   dt.vdt1.var_f += 2.0;
   dt.vdt1.var_g += 2;
   dt.vdt1.var_h += 2.0f;

   dtp = &dt;
   return(*dtp);
}

DT2 fun3(DT2 *dtx, DT2 *dty) {

   if ( dtx->var_a != 4.0f || dtx->vdt1.var_a != 4 ) exit(53);
   if ( dtx->var_b != 8 || dtx->vdt1.var_b != 8.0 ) exit(55);
   if ( dtx->var_c != 12.0l || dtx->vdt1.var_c != 12 ) exit(57);
   if ( dtx->var_d != 16.0 || dtx->vdt1.var_d != 16.0f ) exit(59);
   if ( dtx->var_e != 20.0f || dtx->vdt1.var_e != 20.0l ) exit(61);
   if ( dtx->var_f != 24 || dtx->vdt1.var_f != 24.0 ) exit(63);
   if ( dtx->var_g != 28.0 || dtx->vdt1.var_g != 28 ) exit(65);
   if ( dtx->var_h != 32 || dtx->vdt1.var_h != 32.0f ) exit(67);

   dty->var_a = dtx->var_a + 1.0f;
   dty->var_b = dtx->var_b + 1;
   dty->var_c = dtx->var_c + 1.0l;
   dty->var_d = dtx->var_d + 1.0;
   dty->var_e = dtx->var_e + 1.0f;
   dty->var_f = dtx->var_f + 1;
   dty->var_g = dtx->var_g + 1.0;
   dty->var_h = dtx->var_h + 1;

   dty->vdt1.var_a = dtx->vdt1.var_a + 2;
   dty->vdt1.var_b = dtx->vdt1.var_b + 2.0;
   dty->vdt1.var_c = dtx->vdt1.var_c + 2;
   dty->vdt1.var_d = dtx->vdt1.var_d + 2.0f;
   dty->vdt1.var_e = dtx->vdt1.var_e + 2.0l;
   dty->vdt1.var_f = dtx->vdt1.var_f + 2.0;
   dty->vdt1.var_g = dtx->vdt1.var_g + 2;
   dty->vdt1.var_h = dtx->vdt1.var_h + 2.0f;

   return(*dty);
}

DT2 *fun4(DT2 *dtx, DT2 *dty) {

   DT2 *dtz;

   dtz = malloc(sizeof(DT2));

   if ( dtx->var_a != 4.0f || dtx->vdt1.var_a != 4 ) exit(69);
   if ( dtx->var_b != 8 || dtx->vdt1.var_b != 8.0 ) exit(71);
   if ( dtx->var_c != 12.0l || dtx->vdt1.var_c != 12 ) exit(73);
   if ( dtx->var_d != 16.0 || dtx->vdt1.var_d != 16.0f ) exit(75);
   if ( dtx->var_e != 20.0f || dtx->vdt1.var_e != 20.0l ) exit(77);
   if ( dtx->var_f != 24 || dtx->vdt1.var_f != 24.0 ) exit(79);
   if ( dtx->var_g != 28.0 || dtx->vdt1.var_g != 28 ) exit(81);
   if ( dtx->var_h != 32 || dtx->vdt1.var_h != 32.0f ) exit(83);

   dtz->var_a = dtx->var_a + 1.0f;
   dtz->var_b = dtx->var_b + 1;
   dtz->var_c = dtx->var_c + 1.0l;
   dtz->var_d = dtx->var_d + 1.0;
   dtz->var_e = dtx->var_e + 1.0f;
   dtz->var_f = dtx->var_f + 1;
   dtz->var_g = dtx->var_g + 1.0;
   dtz->var_h = dtx->var_h + 1;

   dtz->vdt1.var_a = dtx->vdt1.var_a + 2;
   dtz->vdt1.var_b = dtx->vdt1.var_b + 2.0;
   dtz->vdt1.var_c = dtx->vdt1.var_c + 2;
   dtz->vdt1.var_d = dtx->vdt1.var_d + 2.0f;
   dtz->vdt1.var_e = dtx->vdt1.var_e + 2.0l;
   dtz->vdt1.var_f = dtx->vdt1.var_f + 2.0;
   dtz->vdt1.var_g = dtx->vdt1.var_g + 2;
   dtz->vdt1.var_h = dtx->vdt1.var_h + 2.0f;

   dty = dtz;
   return(dtz);
}

