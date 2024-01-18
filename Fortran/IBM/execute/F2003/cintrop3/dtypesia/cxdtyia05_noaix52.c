
#include <stdio.h>
#include <stdlib.h>

   #include <inttypes.h>
   #include <stddef.h>

  #define int_fast16_t short

typedef struct dt1 DT1;
typedef struct dt2 DT2;
typedef struct dt3 DT3;

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

struct dt3 {
   DT2 vdt2;
   size_t var_a;
   int_least8_t var_b;
   int_least64_t var_c;
   signed char var_d;
   intptr_t var_e;
   int_least16_t var_f;
   intmax_t var_g;
   int_least32_t var_h;
};

DT3 fun1(DT3 *dt) {

   if ( dt->var_a != 2 || dt->vdt2.var_a != 2 || 
                          dt->vdt2.vdt1.var_a != 2 ) exit(21);
   if ( dt->var_b != 4 || dt->vdt2.var_b != 4 || 
                          dt->vdt2.vdt1.var_b != 4 ) exit(23);
   if ( dt->var_c != 6 || dt->vdt2.var_c != 6 ||
                          dt->vdt2.vdt1.var_c != 6 ) exit(25);
   if ( dt->var_d != 8 || dt->vdt2.var_d != 8 ||
                          dt->vdt2.vdt1.var_d != 8 ) exit(27);
   if ( dt->var_e != 10 || dt->vdt2.var_e != 10 ||
                           dt->vdt2.vdt1.var_e != 10 ) exit(29);
   if ( dt->var_f != 12 || dt->vdt2.var_f != 12 ||
                           dt->vdt2.vdt1.var_f != 12 ) exit(31);
   if ( dt->var_g != 14 || dt->vdt2.var_g != 14 ||
                           dt->vdt2.vdt1.var_h != 16 ) exit(33);
   if ( dt->var_h != 16 || dt->vdt2.var_h != 16 ||
                           dt->vdt2.vdt1.var_h != 16 ) exit(35);

   dt->var_a += 1;
   dt->var_b += 1;
   dt->var_c += 1;
   dt->var_d += 1;
   dt->var_e += 1;
   dt->var_f += 1;
   dt->var_g += 1;
   dt->var_h += 1;

   dt->vdt2.var_a += 2;
   dt->vdt2.var_b += 2;
   dt->vdt2.var_c += 2;
   dt->vdt2.var_d += 2;
   dt->vdt2.var_e += 2;
   dt->vdt2.var_f += 2;
   dt->vdt2.var_g += 2;
   dt->vdt2.var_h += 2;

   dt->vdt2.vdt1.var_a += 3;
   dt->vdt2.vdt1.var_b += 3;
   dt->vdt2.vdt1.var_c += 3;
   dt->vdt2.vdt1.var_d += 3;
   dt->vdt2.vdt1.var_e += 3;
   dt->vdt2.vdt1.var_f += 3;
   dt->vdt2.vdt1.var_g += 3;
   dt->vdt2.vdt1.var_h += 3;

   return(*dt);
}

DT3 fun2(DT3 dt) {

   DT3 *dtp;

   dtp = malloc(sizeof(DT3));

   if ( dt.var_a != 2 || dt.vdt2.var_a != 2 || 
                          dt.vdt2.vdt1.var_a != 2 ) exit(37);
   if ( dt.var_b != 4 || dt.vdt2.var_b != 4 || 
                          dt.vdt2.vdt1.var_b != 4 ) exit(39);
   if ( dt.var_c != 6 || dt.vdt2.var_c != 6 ||
                          dt.vdt2.vdt1.var_c != 6 ) exit(41);
   if ( dt.var_d != 8 || dt.vdt2.var_d != 8 ||
                          dt.vdt2.vdt1.var_d != 8 ) exit(43);
   if ( dt.var_e != 10 || dt.vdt2.var_e != 10 ||
                           dt.vdt2.vdt1.var_e != 10 ) exit(45);
   if ( dt.var_f != 12 || dt.vdt2.var_f != 12 ||
                           dt.vdt2.vdt1.var_f != 12 ) exit(47);
   if ( dt.var_g != 14 || dt.vdt2.var_g != 14 ||
                           dt.vdt2.vdt1.var_h != 16 ) exit(49);
   if ( dt.var_h != 16 || dt.vdt2.var_h != 16 ||
                           dt.vdt2.vdt1.var_h != 16 ) exit(51);

   dt.var_a += 1;
   dt.var_b += 1;
   dt.var_c += 1;
   dt.var_d += 1;
   dt.var_e += 1;
   dt.var_f += 1;
   dt.var_g += 1;
   dt.var_h += 1;

   dt.vdt2.var_a += 2;
   dt.vdt2.var_b += 2;
   dt.vdt2.var_c += 2;
   dt.vdt2.var_d += 2;
   dt.vdt2.var_e += 2;
   dt.vdt2.var_f += 2;
   dt.vdt2.var_g += 2;
   dt.vdt2.var_h += 2;

   dt.vdt2.vdt1.var_a += 3;
   dt.vdt2.vdt1.var_b += 3;
   dt.vdt2.vdt1.var_c += 3;
   dt.vdt2.vdt1.var_d += 3;
   dt.vdt2.vdt1.var_e += 3;
   dt.vdt2.vdt1.var_f += 3;
   dt.vdt2.vdt1.var_g += 3;
   dt.vdt2.vdt1.var_h += 3;

   dtp = &dt;
   return(*dtp);
}

DT3 fun3(DT3 *dtx, DT3 *dty) {

   if ( dtx->var_a != 4 || dtx->vdt2.var_a != 4 ||
                           dtx->vdt2.vdt1.var_a != 4 ) exit(53);

   if ( dtx->var_b != 8 || dtx->vdt2.var_b != 8 ||
                           dtx->vdt2.vdt1.var_b != 8 ) exit(55);

   if ( dtx->var_c != 12 || dtx->vdt2.var_c != 12 ||
                            dtx->vdt2.vdt1.var_c != 12 ) exit(57);

   if ( dtx->var_d != 16 || dtx->vdt2.var_d != 16 ||
                            dtx->vdt2.vdt1.var_d != 16 ) exit(59);

   if ( dtx->var_e != 20 || dtx->vdt2.var_e != 20 ||
                            dtx->vdt2.vdt1.var_e != 20 ) exit(61);

   if ( dtx->var_f != 24 || dtx->vdt2.var_f != 24 ||
                            dtx->vdt2.vdt1.var_f != 24 ) exit(63);

   if ( dtx->var_g != 28 || dtx->vdt2.var_g != 28 ||
                            dtx->vdt2.vdt1.var_g != 28 ) exit(65);

   if ( dtx->var_h != 32 || dtx->vdt2.var_h != 32 ||
                            dtx->vdt2.vdt1.var_h != 32 ) exit(67);

   dty->var_a = dtx->var_a + 1;
   dty->var_b = dtx->var_b + 1;
   dty->var_c = dtx->var_c + 1;
   dty->var_d = dtx->var_d + 1;
   dty->var_e = dtx->var_e + 1;
   dty->var_f = dtx->var_f + 1;
   dty->var_g = dtx->var_g + 1;
   dty->var_h = dtx->var_h + 1;

   dty->vdt2.var_a = dtx->vdt2.var_a + 2;
   dty->vdt2.var_b = dtx->vdt2.var_b + 2;
   dty->vdt2.var_c = dtx->vdt2.var_c + 2;
   dty->vdt2.var_d = dtx->vdt2.var_d + 2;
   dty->vdt2.var_e = dtx->vdt2.var_e + 2;
   dty->vdt2.var_f = dtx->vdt2.var_f + 2;
   dty->vdt2.var_g = dtx->vdt2.var_g + 2;
   dty->vdt2.var_h = dtx->vdt2.var_h + 2;

   dty->vdt2.vdt1.var_a = dtx->vdt2.vdt1.var_a + 3;
   dty->vdt2.vdt1.var_b = dtx->vdt2.vdt1.var_b + 3;
   dty->vdt2.vdt1.var_c = dtx->vdt2.vdt1.var_c + 3;
   dty->vdt2.vdt1.var_d = dtx->vdt2.vdt1.var_d + 3;
   dty->vdt2.vdt1.var_e = dtx->vdt2.vdt1.var_e + 3;
   dty->vdt2.vdt1.var_f = dtx->vdt2.vdt1.var_f + 3;
   dty->vdt2.vdt1.var_g = dtx->vdt2.vdt1.var_g + 3;
   dty->vdt2.vdt1.var_h = dtx->vdt2.vdt1.var_h + 3;

   return(*dty);
}

DT3 *fun4(DT3 *dtx, DT3 *dty) {

   DT3 *dtz;

   dtz = malloc(sizeof(DT3));

   if ( dtx->var_a != 4 || dtx->vdt2.var_a != 4 ||
                           dtx->vdt2.vdt1.var_a != 4 ) exit(69);

   if ( dtx->var_b != 8 || dtx->vdt2.var_b != 8 ||
                           dtx->vdt2.vdt1.var_b != 8 ) exit(71);

   if ( dtx->var_c != 12 || dtx->vdt2.var_c != 12 ||
                            dtx->vdt2.vdt1.var_c != 12 ) exit(73);

   if ( dtx->var_d != 16 || dtx->vdt2.var_d != 16 ||
                            dtx->vdt2.vdt1.var_d != 16 ) exit(75);

   if ( dtx->var_e != 20 || dtx->vdt2.var_e != 20 ||
                            dtx->vdt2.vdt1.var_e != 20 ) exit(77);

   if ( dtx->var_f != 24 || dtx->vdt2.var_f != 24 ||
                            dtx->vdt2.vdt1.var_f != 24 ) exit(79);

   if ( dtx->var_g != 28 || dtx->vdt2.var_g != 28 ||
                            dtx->vdt2.vdt1.var_g != 28 ) exit(81);

   if ( dtx->var_h != 32 || dtx->vdt2.var_h != 32 ||
                            dtx->vdt2.vdt1.var_h != 32 ) exit(83);

   dtz->var_a = dtx->var_a + 1;
   dtz->var_b = dtx->var_b + 1;
   dtz->var_c = dtx->var_c + 1;
   dtz->var_d = dtx->var_d + 1;
   dtz->var_e = dtx->var_e + 1;
   dtz->var_f = dtx->var_f + 1;
   dtz->var_g = dtx->var_g + 1;
   dtz->var_h = dtx->var_h + 1;

   dtz->vdt2.var_a = dtx->vdt2.var_a + 2;
   dtz->vdt2.var_b = dtx->vdt2.var_b + 2;
   dtz->vdt2.var_c = dtx->vdt2.var_c + 2;
   dtz->vdt2.var_d = dtx->vdt2.var_d + 2;
   dtz->vdt2.var_e = dtx->vdt2.var_e + 2;
   dtz->vdt2.var_f = dtx->vdt2.var_f + 2;
   dtz->vdt2.var_g = dtx->vdt2.var_g + 2;
   dtz->vdt2.var_h = dtx->vdt2.var_h + 2;

   dtz->vdt2.vdt1.var_a = dtx->vdt2.vdt1.var_a + 3;
   dtz->vdt2.vdt1.var_b = dtx->vdt2.vdt1.var_b + 3;
   dtz->vdt2.vdt1.var_c = dtx->vdt2.vdt1.var_c + 3;
   dtz->vdt2.vdt1.var_d = dtx->vdt2.vdt1.var_d + 3;
   dtz->vdt2.vdt1.var_e = dtx->vdt2.vdt1.var_e + 3;
   dtz->vdt2.vdt1.var_f = dtx->vdt2.vdt1.var_f + 3;
   dtz->vdt2.vdt1.var_g = dtx->vdt2.vdt1.var_g + 3;
   dtz->vdt2.vdt1.var_h = dtx->vdt2.vdt1.var_h + 3;

   dty = dtz;
   return(dtz);
}

