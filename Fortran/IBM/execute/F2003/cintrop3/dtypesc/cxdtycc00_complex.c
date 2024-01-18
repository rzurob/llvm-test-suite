
#include <stdio.h>
#include <stdlib.h>

   #include <complex.h>

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

   if ( dt->var_a != 2.0f+I*2.0f ) exit(21);
   if ( dt->var_b != 4 ) exit(23);
   if ( dt->var_c != 6.0l+I*6.0l ) exit(25);
   if ( dt->var_d != 8.0+I*8.0 ) exit(27);
   if ( dt->var_e != 10.0f+I*10.0f ) exit(29);
   if ( dt->var_f != 12 ) exit(31);
   if ( dt->var_g != 14.0+I*14.0 ) exit(33);
   if ( dt->var_h != 16 ) exit(35);

   dt->var_a += 1.0f+I*1.0f;
   dt->var_b += 1;
   dt->var_c += 1.0l+I*1.0l;
   dt->var_d += 1.0+I*1.0;
   dt->var_e += 1.0f+I*1.0f;
   dt->var_f += 1;
   dt->var_g += 1.0+I*1.0;
   dt->var_h += 1;
}

void sub2(DT1 dt) {

   if ( dt.var_a != 2.0f+I*2.0f ) exit(37);
   if ( dt.var_b != 4 ) exit(39);
   if ( dt.var_c != 6.0l+I*6.0l ) exit(41);
   if ( dt.var_d != 8.0+I*8.0 ) exit(43);
   if ( dt.var_e != 10.0f+I*10.0f ) exit(45);
   if ( dt.var_f != 12 ) exit(47);
   if ( dt.var_g != 14.0+I*14.0 ) exit(49);
   if ( dt.var_h != 16 ) exit(51);

   dt.var_a += 1.0f+I*1.0f;
   dt.var_b += 1;
   dt.var_c += 1.0l+I*1.0l;
   dt.var_d += 1.0+I*1.0;
   dt.var_e += 1.0f+I*1.0f;
   dt.var_f += 1;
   dt.var_g += 1.0+I*1.0;
   dt.var_h += 1;
}

void sub3(DT1 *dtx, DT1 *dty) {

   if ( dtx->var_a != 4.0f+I*4.0f ) exit(53);
   if ( dtx->var_b != 8 ) exit(55);
   if ( dtx->var_c != 12.0l+I*12.0l ) exit(57);
   if ( dtx->var_d != 16.0+I*16.0 ) exit(59);
   if ( dtx->var_e != 20.0f+I*20.0f ) exit(61);
   if ( dtx->var_f != 24 ) exit(63);
   if ( dtx->var_g != 28.0+I*28.0 ) exit(65);
   if ( dtx->var_h != 32 ) exit(67);

   dty->var_a = dtx->var_a + 1.0f+I*1.0f;
   dty->var_b = dtx->var_b + 1;
   dty->var_c = dtx->var_c + 1.0l+I*1.0l;
   dty->var_d = dtx->var_d + 1.0+I*1.0;
   dty->var_e = dtx->var_e + 1.0f+I*1.0f;
   dty->var_f = dtx->var_f + 1;
   dty->var_g = dtx->var_g + 1.0+I*1.0;
   dty->var_h = dtx->var_h + 1;
   
}
