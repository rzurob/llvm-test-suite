
#include <stdio.h>
#include <stdlib.h>

typedef struct dt1 DT1;

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

DT1 fun1(DT1 *dt) {

   if ( dt->var_a != 2 ) exit(21);
   if ( dt->var_b != 4 ) exit(23);
   if ( dt->var_c != 6 ) exit(25);
   if ( dt->var_d != 8 ) exit(27);
   if ( dt->var_e != 10 ) exit(29);
   if ( dt->var_f != 12 ) exit(31);
   if ( dt->var_g != 14 ) exit(33);
   if ( dt->var_h != 16 ) exit(35);

   dt->var_a += 1;
   dt->var_b += 1;
   dt->var_c += 1;
   dt->var_d += 1;
   dt->var_e += 1;
   dt->var_f += 1;
   dt->var_g += 1;
   dt->var_h += 1;

   return(*dt);
}

DT1 fun2(DT1 dt) {

   DT1 *dtp;

   dtp = malloc(sizeof(DT1));

   if ( dt.var_a != 2 ) exit(37);
   if ( dt.var_b != 4 ) exit(39);
   if ( dt.var_c != 6 ) exit(41);
   if ( dt.var_d != 8 ) exit(43);
   if ( dt.var_e != 10 ) exit(45);
   if ( dt.var_f != 12 ) exit(47);
   if ( dt.var_g != 14 ) exit(49);
   if ( dt.var_h != 16 ) exit(51);

   dt.var_a += 1;
   dt.var_b += 1;
   dt.var_c += 1;
   dt.var_d += 1;
   dt.var_e += 1;
   dt.var_f += 1;
   dt.var_g += 1;
   dt.var_h += 1;

   dtp = &dt;
   return(*dtp);
}

DT1 fun3(DT1 *dtx, DT1 *dty) {

   if ( dtx->var_a != 4 ) exit(53);
   if ( dtx->var_b != 8 ) exit(55);
   if ( dtx->var_c != 12 ) exit(57);
   if ( dtx->var_d != 16 ) exit(59);
   if ( dtx->var_e != 20 ) exit(61);
   if ( dtx->var_f != 24 ) exit(63);
   if ( dtx->var_g != 28 ) exit(65);
   if ( dtx->var_h != 32 ) exit(67);

   dty->var_a = dtx->var_a + 1;
   dty->var_b = dtx->var_b + 1;
   dty->var_c = dtx->var_c + 1;
   dty->var_d = dtx->var_d + 1;
   dty->var_e = dtx->var_e + 1;
   dty->var_f = dtx->var_f + 1;
   dty->var_g = dtx->var_g + 1;
   dty->var_h = dtx->var_h + 1;
   
   return(*dty);
}

DT1 *fun4(DT1 *dtx, DT1 *dty) {

   DT1 *dtz;

   dtz = malloc(sizeof(DT1));

   if ( dtx->var_a != 4 ) exit(69);
   if ( dtx->var_b != 8 ) exit(71);
   if ( dtx->var_c != 12 ) exit(73);
   if ( dtx->var_d != 16 ) exit(75);
   if ( dtx->var_e != 20 ) exit(77);
   if ( dtx->var_f != 24 ) exit(79);
   if ( dtx->var_g != 28 ) exit(81);
   if ( dtx->var_h != 32 ) exit(83);

   dty->var_a = dtx->var_a + 1;
   dty->var_b = dtx->var_b + 1;
   dty->var_c = dtx->var_c + 1;
   dty->var_d = dtx->var_d + 1;
   dty->var_e = dtx->var_e + 1;
   dty->var_f = dtx->var_f + 1;
   dty->var_g = dtx->var_g + 1;
   dty->var_h = dtx->var_h + 1;
   
   dtz->var_a = dty->var_a + 1;
   dtz->var_b = dty->var_b + 1;
   dtz->var_c = dty->var_c + 1;
   dtz->var_d = dty->var_d + 1;
   dtz->var_e = dty->var_e + 1;
   dtz->var_f = dty->var_f + 1;
   dtz->var_g = dty->var_g + 1;
   dtz->var_h = dty->var_h + 1;
   
   return(dtz);
}
