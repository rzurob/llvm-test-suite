
#include <stdio.h>
#include <stdlib.h>



typedef struct dt1 DT1;
typedef struct dt2 DT2;
typedef struct dt3 DT3;

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
   double var_a;
   _Bool var_b;
   double var_c;
   char var_d;
   long double var_e;
   float var_f;
   _Bool var_g;
   float var_h;
   DT1 vdt1;
};

struct dt3 {
   DT2 vdt2;
   float var_a;
   _Bool var_b;
   long double var_c;
   double var_d;
   float var_e;
   _Bool var_f;
   double var_g;
   char var_h;
};

DT3 fun1(DT3 *dt) {

   if ( dt->var_a != 2.0f || dt->vdt2.var_a != 2.0 || 
                          dt->vdt2.vdt1.var_a != 1 ) exit(21);
   if ( dt->var_b != 1 || dt->vdt2.var_b != 1 || 
                          dt->vdt2.vdt1.var_b != 4.0 ) exit(23);
   if ( dt->var_c != 6.0l || dt->vdt2.var_c != 6.0 ||
                          dt->vdt2.vdt1.var_c != 'A' ) exit(25);
   if ( dt->var_d != 8.0 || dt->vdt2.var_d != 'A' ||
                          dt->vdt2.vdt1.var_d != 8.0f ) exit(27);
   if ( dt->var_e != 10.0f || dt->vdt2.var_e != 10.0l ||
                           dt->vdt2.vdt1.var_e != 10.0l ) exit(29);
   if ( dt->var_f != 1 || dt->vdt2.var_f != 12.0f ||
                           dt->vdt2.vdt1.var_f != 12.0 ) exit(31);
   if ( dt->var_g != 14.0 || dt->vdt2.var_g != 1 ||
                           dt->vdt2.vdt1.var_h != 16.0f ) exit(33);
   if ( dt->var_h != 'A' || dt->vdt2.var_h != 16.0f ||
                           dt->vdt2.vdt1.var_h != 16.0f ) exit(35);

   dt->var_a += 1.0f;
   dt->var_b = 0;
   dt->var_c += 1.0l;
   dt->var_d += 1.0;
   dt->var_e += 1.0f;
   dt->var_f = 0;
   dt->var_g += 1.0;
   dt->var_h = 'B';

   dt->vdt2.var_a += 2.0;
   dt->vdt2.var_b = 0;
   dt->vdt2.var_c += 2.0;
   dt->vdt2.var_d = 'B';
   dt->vdt2.var_e += 2.0l;
   dt->vdt2.var_f += 2.0f;
   dt->vdt2.var_g = 0;
   dt->vdt2.var_h += 2.0f;

   dt->vdt2.vdt1.var_a = 0;
   dt->vdt2.vdt1.var_b += 3.0;
   dt->vdt2.vdt1.var_c = 'B';
   dt->vdt2.vdt1.var_d += 3.0f;
   dt->vdt2.vdt1.var_e += 3.0l;
   dt->vdt2.vdt1.var_f += 3.0;
   dt->vdt2.vdt1.var_g = 0;
   dt->vdt2.vdt1.var_h += 3.0f;

   return(*dt);
}

DT3 fun2(DT3 dt) {

   DT3 *dtp;

   dtp = malloc(sizeof(DT3));

   if ( dt.var_a != 2.0f || dt.vdt2.var_a != 2.0 || 
                          dt.vdt2.vdt1.var_a != 1 ) exit(37);
   if ( dt.var_b != 1 || dt.vdt2.var_b != 1 || 
                          dt.vdt2.vdt1.var_b != 4.0 ) exit(39);
   if ( dt.var_c != 6.0l || dt.vdt2.var_c != 6.0 ||
                          dt.vdt2.vdt1.var_c != 'A' ) exit(41);
   if ( dt.var_d != 8.0 || dt.vdt2.var_d != 'A' ||
                          dt.vdt2.vdt1.var_d != 8.0f ) exit(43);
   if ( dt.var_e != 10.0f || dt.vdt2.var_e != 10.0l ||
                           dt.vdt2.vdt1.var_e != 10.0l ) exit(45);
   if ( dt.var_f != 1 || dt.vdt2.var_f != 12.0f ||
                           dt.vdt2.vdt1.var_f != 12.0 ) exit(47);
   if ( dt.var_g != 14.0 || dt.vdt2.var_g != 1 ||
                           dt.vdt2.vdt1.var_h != 16.0f ) exit(49);
   if ( dt.var_h != 'A' || dt.vdt2.var_h != 16.0f ||
                           dt.vdt2.vdt1.var_h != 16.0f ) exit(51);

   dt.var_a += 1.0f;
   dt.var_b = 0;
   dt.var_c += 1.0l;
   dt.var_d += 1.0;
   dt.var_e += 1.0f;
   dt.var_f = 0;
   dt.var_g += 1.0;
   dt.var_h = 'B';

   dt.vdt2.var_a += 2.0;
   dt.vdt2.var_b = 0;
   dt.vdt2.var_c += 2.0;
   dt.vdt2.var_d = 'B';
   dt.vdt2.var_e += 2.0l;
   dt.vdt2.var_f += 2.0f;
   dt.vdt2.var_g = 0;
   dt.vdt2.var_h += 2.0f;

   dt.vdt2.vdt1.var_a = 0;
   dt.vdt2.vdt1.var_b += 3.0;
   dt.vdt2.vdt1.var_c = 'B';
   dt.vdt2.vdt1.var_d += 3.0f;
   dt.vdt2.vdt1.var_e += 3.0l;
   dt.vdt2.vdt1.var_f += 3.0;
   dt.vdt2.vdt1.var_g = 0;
   dt.vdt2.vdt1.var_h += 3.0f;

   dtp = &dt;
   return(*dtp);
}

DT3 fun3(DT3 *dtx, DT3 *dty) {

   if ( dtx->var_a != 4.0f || dtx->vdt2.var_a != 4.0 ||
                           dtx->vdt2.vdt1.var_a != 1 ) exit(53);

   if ( dtx->var_b != 1 || dtx->vdt2.var_b != 1 ||
                           dtx->vdt2.vdt1.var_b != 8.0 ) exit(55);

   if ( dtx->var_c != 12.0l || dtx->vdt2.var_c != 12.0 ||
                            dtx->vdt2.vdt1.var_c != 'A' ) exit(57);

   if ( dtx->var_d != 16.0 || dtx->vdt2.var_d != 'A' ||
                            dtx->vdt2.vdt1.var_d != 16.0f ) exit(59);

   if ( dtx->var_e != 20.0f || dtx->vdt2.var_e != 20.0l ||
                            dtx->vdt2.vdt1.var_e != 20.0l ) exit(61);

   if ( dtx->var_f != 1 || dtx->vdt2.var_f != 24.0f ||
                            dtx->vdt2.vdt1.var_f != 24.0 ) exit(63);

   if ( dtx->var_g != 28.0 || dtx->vdt2.var_g != 1 ||
                            dtx->vdt2.vdt1.var_g != 1 ) exit(65);

   if ( dtx->var_h != 'A' || dtx->vdt2.var_h != 32.0f ||
                            dtx->vdt2.vdt1.var_h != 32.0f ) exit(67);

   dty->var_a = dtx->var_a + 1.0f;
   dty->var_b = ! dtx->var_b;
   dty->var_c = dtx->var_c + 1.0l;
   dty->var_d = dtx->var_d + 1.0;
   dty->var_e = dtx->var_e + 1.0f;
   dty->var_f = ! dtx->var_f;
   dty->var_g = dtx->var_g + 1.0;
   dty->var_h = 'B';

   dty->vdt2.var_a = dtx->vdt2.var_a + 2.0;
   dty->vdt2.var_b = ! dtx->vdt2.var_b;
   dty->vdt2.var_c = dtx->vdt2.var_c + 2.0;
   dty->vdt2.var_d = 'B';
   dty->vdt2.var_e = dtx->vdt2.var_e + 2.0l;
   dty->vdt2.var_f = dtx->vdt2.var_f + 2.0f;
   dty->vdt2.var_g = ! dtx->vdt2.var_g;
   dty->vdt2.var_h = dtx->vdt2.var_h + 2.0f;

   dty->vdt2.vdt1.var_a = ! dtx->vdt2.vdt1.var_a;
   dty->vdt2.vdt1.var_b = dtx->vdt2.vdt1.var_b + 3.0;
   dty->vdt2.vdt1.var_c = 'B';
   dty->vdt2.vdt1.var_d = dtx->vdt2.vdt1.var_d + 3.0f;
   dty->vdt2.vdt1.var_e = dtx->vdt2.vdt1.var_e + 3.0l;
   dty->vdt2.vdt1.var_f = dtx->vdt2.vdt1.var_f + 3.0;
   dty->vdt2.vdt1.var_g = ! dtx->vdt2.vdt1.var_g;
   dty->vdt2.vdt1.var_h = dtx->vdt2.vdt1.var_h + 3.0f;

   return(*dty);
}

DT3 *fun4(DT3 *dtx, DT3 *dty) {

   DT3 *dtz;

   dtz = malloc(sizeof(DT3));

   if ( dtx->var_a != 4.0f || dtx->vdt2.var_a != 4.0 ||
                           dtx->vdt2.vdt1.var_a != 1 ) exit(69);

   if ( dtx->var_b != 1 || dtx->vdt2.var_b != 1 ||
                           dtx->vdt2.vdt1.var_b != 8.0 ) exit(71);

   if ( dtx->var_c != 12.0l || dtx->vdt2.var_c != 12.0 ||
                            dtx->vdt2.vdt1.var_c != 'A' ) exit(73);

   if ( dtx->var_d != 16.0 || dtx->vdt2.var_d != 'A' ||
                            dtx->vdt2.vdt1.var_d != 16.0f ) exit(75);

   if ( dtx->var_e != 20.0f || dtx->vdt2.var_e != 20.0l ||
                            dtx->vdt2.vdt1.var_e != 20.0l ) exit(77);

   if ( dtx->var_f != 1 || dtx->vdt2.var_f != 24.0f ||
                            dtx->vdt2.vdt1.var_f != 24.0 ) exit(79);

   if ( dtx->var_g != 28.0 || dtx->vdt2.var_g != 1 ||
                            dtx->vdt2.vdt1.var_g != 1 ) exit(81);

   if ( dtx->var_h != 'A' || dtx->vdt2.var_h != 32.0f ||
                            dtx->vdt2.vdt1.var_h != 32.0f ) exit(83);

   dtz->var_a = dtx->var_a + 1.0f;
   dtz->var_b = ! dtx->var_b;
   dtz->var_c = dtx->var_c + 1.0l;
   dtz->var_d = dtx->var_d + 1.0;
   dtz->var_e = dtx->var_e + 1.0f;
   dtz->var_f = ! dtx->var_f;
   dtz->var_g = dtx->var_g + 1.0;
   dtz->var_h = 'B';

   dtz->vdt2.var_a = dtx->vdt2.var_a + 2.0;
   dtz->vdt2.var_b = ! dtx->vdt2.var_b;
   dtz->vdt2.var_c = dtx->vdt2.var_c + 2.0;
   dtz->vdt2.var_d = 'B';
   dtz->vdt2.var_e = dtx->vdt2.var_e + 2.0l;
   dtz->vdt2.var_f = dtx->vdt2.var_f + 2.0f;
   dtz->vdt2.var_g = ! dtx->vdt2.var_g;
   dtz->vdt2.var_h = dtx->vdt2.var_h + 2.0f;

   dtz->vdt2.vdt1.var_a = ! dtx->vdt2.vdt1.var_a;
   dtz->vdt2.vdt1.var_b = dtx->vdt2.vdt1.var_b + 3.0;
   dtz->vdt2.vdt1.var_c = 'B';
   dtz->vdt2.vdt1.var_d = dtx->vdt2.vdt1.var_d + 3.0f;
   dtz->vdt2.vdt1.var_e = dtx->vdt2.vdt1.var_e + 3.0l;
   dtz->vdt2.vdt1.var_f = dtx->vdt2.vdt1.var_f + 3.0;
   dtz->vdt2.vdt1.var_g = ! dtx->vdt2.vdt1.var_g;
   dtz->vdt2.vdt1.var_h = dtx->vdt2.vdt1.var_h + 3.0f;

   dty = dtz;
   return(dtz);
}

