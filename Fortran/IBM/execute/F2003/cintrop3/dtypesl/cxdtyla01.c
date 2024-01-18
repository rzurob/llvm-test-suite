
#include <stdio.h>
#include <stdlib.h>

typedef struct dt1 DT1;

struct dt1 {
   float var_a;
   _Bool var_b;
   long double var_c;
   double var_d;
   float var_e;
   _Bool var_f;
   double var_g;
   char var_h;
};

DT1 fun1(DT1 *dt) {

   if ( dt->var_a != 2.0f ) exit(21);
   if ( dt->var_b != 1 ) exit(23);
   if ( dt->var_c != 6.0l ) exit(25);
   if ( dt->var_d != 8.0 ) exit(27);
   if ( dt->var_e != 10.0f ) exit(29);
   if ( dt->var_f != 1 ) exit(31);
   if ( dt->var_g != 14.0 ) exit(33);
   if ( dt->var_h != 'A' ) exit(35);

   dt->var_a += 1.0f;
   dt->var_b = 0;
   dt->var_c += 1.0l;
   dt->var_d += 1.0;
   dt->var_e += 1.0f;
   dt->var_f = 0;
   dt->var_g += 1.0;
   dt->var_h = 'B';

   return(*dt);
}

DT1 fun2(DT1 dt) {

   DT1 *dtp;

   dtp = malloc(sizeof(DT1));

   if ( dt.var_a != 2.0f ) exit(37);
   if ( dt.var_b != 1 ) exit(39);
   if ( dt.var_c != 6.0l ) exit(41);
   if ( dt.var_d != 8.0 ) exit(43);
   if ( dt.var_e != 10.0f ) exit(45);
   if ( dt.var_f != 1 ) exit(47);
   if ( dt.var_g != 14.0 ) exit(49);
   if ( dt.var_h != 'A' ) exit(51);

   dt.var_a += 1.0f;
   dt.var_b = 0;
   dt.var_c += 1.0l;
   dt.var_d += 1.0;
   dt.var_e += 1.0f;
   dt.var_f = 0;
   dt.var_g += 1.0;
   dt.var_h = 'B';

   dtp = &dt;
   return(*dtp);
}

DT1 fun3(DT1 *dtx, DT1 *dty) {

   if ( dtx->var_a != 4.0f ) exit(53);
   if ( dtx->var_b != 1 ) exit(55);
   if ( dtx->var_c != 12.0l ) exit(57);
   if ( dtx->var_d != 16.0 ) exit(59);
   if ( dtx->var_e != 20.0f ) exit(61);
   if ( dtx->var_f != 1 ) exit(63);
   if ( dtx->var_g != 28.0 ) exit(65);
   if ( dtx->var_h != 'A' ) exit(67);

   dty->var_a = dtx->var_a + 1.0f;
   dty->var_b = ! dtx->var_b;
   dty->var_c = dtx->var_c + 1.0l;
   dty->var_d = dtx->var_d + 1.0;
   dty->var_e = dtx->var_e + 1.0f;
   dty->var_f = ! dtx->var_f;
   dty->var_g = dtx->var_g + 1.0;
   dty->var_h = 'B';
   
   return(*dty);
}

DT1 *fun4(DT1 *dtx, DT1 *dty) {

   DT1 *dtz;

   dtz = malloc(sizeof(DT1));

   if ( dtx->var_a != 4.0f ) exit(69);
   if ( dtx->var_b != 1 ) exit(71);
   if ( dtx->var_c != 12.0l ) exit(73);
   if ( dtx->var_d != 16.0 ) exit(75);
   if ( dtx->var_e != 20.0f ) exit(77);
   if ( dtx->var_f != 1 ) exit(79);
   if ( dtx->var_g != 28.0 ) exit(81);
   if ( dtx->var_h != 'A' ) exit(83);

   dty->var_a = dtx->var_a + 1.0f;
   dty->var_b = ! dtx->var_b;
   dty->var_c = dtx->var_c + 1.0l;
   dty->var_d = dtx->var_d + 1.0;
   dty->var_e = dtx->var_e + 1.0f;
   dty->var_f = ! dtx->var_f;
   dty->var_g = dtx->var_g + 1.0;
   dty->var_h = 'B';
   
   dtz->var_a = dty->var_a + 1.0f;
   dtz->var_b = ! dtx->var_b;
   dtz->var_c = dty->var_c + 1.0l;
   dtz->var_d = dty->var_d + 1.0;
   dtz->var_e = dty->var_e + 1.0f;
   dtz->var_f = ! dtx->var_f;
   dtz->var_g = dty->var_g + 1.0;
   dtz->var_h = 'B';
   
   return(dtz);
}
