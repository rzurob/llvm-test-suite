
#include <stdio.h>
#include <stdlib.h>


  #define int_fast16_t short

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

DT1 *st1sum(DT1 *dtx, DT1 *dty);
DT2 *st2sum(DT2 *dtx, DT2 *dty);

int main() {

   void sub1(DT2 *dt);
   void sub2(DT2 dt);
   void sub3(DT2 *dtx, DT2 *dty);

   DT2 dt0 = {2.0f,0,6.0l,8.0,10.0f,0,14.0,'A',
             {0,4.0,'A',8.0f,10.0l,12.0,0,16.0f}};

   DT2 dta, dtb, *dtp;


   dta = dt0;

   sub1(&dta);

   if ( dta.var_a != 3.0f || dta.vdt1.var_a != 1 ) exit(21);
   if ( dta.var_b != 1 || dta.vdt1.var_b != 6 ) exit(23);
   if ( dta.var_c != 7.0l || dta.vdt1.var_c != 'B' ) exit(25);
   if ( dta.var_d != 9.0 || dta.vdt1.var_d != 10 ) exit(27);
   if ( dta.var_e != 11.0f || dta.vdt1.var_e != 12 ) exit(29);
   if ( dta.var_f != 1 || dta.vdt1.var_f != 14 ) exit(31);
   if ( dta.var_g != 15.0 || dta.vdt1.var_g != 1 ) exit(33);
   if ( dta.var_h != 'B' || dta.vdt1.var_h != 18 ) exit(35);


   dta = dt0;

   sub2(dta);

   if ( dta.var_a != 2.0f || dta.vdt1.var_a != 0 ) exit(37);
   if ( dta.var_b != 0 || dta.vdt1.var_b != 4 ) exit(39);
   if ( dta.var_c != 6.0l || dta.vdt1.var_c != 'A' ) exit(41);
   if ( dta.var_d != 8.0 || dta.vdt1.var_d != 8 ) exit(43);
   if ( dta.var_e != 10.0f || dta.vdt1.var_e != 10 ) exit(45);
   if ( dta.var_f != 0 || dta.vdt1.var_f != 12 ) exit(47);
   if ( dta.var_g != 14.0 || dta.vdt1.var_g != 0 ) exit(49);
   if ( dta.var_h != 'A' || dta.vdt1.var_h != 16 ) exit(51);


   dta = dt0;
   dtp = malloc(sizeof(DT2));
   dtp = st2sum(&dta,&dta);

   sub3(dtp,&dtb);

   if ( dtb.var_a != 5.0f || dtb.vdt1.var_a != 1 ) exit(53);
   if ( dtb.var_b != 1 || dtb.vdt1.var_b != 10 ) exit(55);
   if ( dtb.var_c != 13.0l || dtb.vdt1.var_c != 'B' ) exit(57);
   if ( dtb.var_d != 17.0 || dtb.vdt1.var_d != 18 ) exit(59);
   if ( dtb.var_e != 21.0f || dtb.vdt1.var_e != 22 ) exit(61);
   if ( dtb.var_f != 1 || dtb.vdt1.var_f != 26 ) exit(63);
   if ( dtb.var_g != 29.0 || dtb.vdt1.var_g != 1 ) exit(65);
   if ( dtb.var_h != 'B' || dtb.vdt1.var_h != 34 ) exit(67);

   return 0;
}

DT2 *st2sum(DT2 *dtx, DT2 *dty) {
   DT2 *dtp;
   DT1 *dtq;

   dtp = malloc(sizeof(DT2));

   dtp->var_a = dtx->var_a + dty->var_a;
   dtp->var_b = dtx->var_b && dty->var_b;
   dtp->var_c = dtx->var_c + dty->var_c;
   dtp->var_d = dtx->var_d + dty->var_d;
   dtp->var_e = dtx->var_e + dty->var_e;
   dtp->var_f = dtx->var_f && dty->var_f;
   dtp->var_g = dtx->var_g + dty->var_g;
   dtp->var_h = dtx->var_h;
   dtq = st1sum(&dtx->vdt1,&dty->vdt1);
   dtp->vdt1 = *dtq;

   return(dtp);
}

DT1 *st1sum(DT1 *dtx, DT1 *dty) {
   DT1 *dtp;

   dtp = malloc(sizeof(DT1));

   dtp->var_a = dtx->var_a && dty->var_a;
   dtp->var_b = dtx->var_b + dty->var_b;
   dtp->var_c = dtx->var_c;
   dtp->var_d = dtx->var_d + dty->var_d;
   dtp->var_e = dtx->var_e + dty->var_e;
   dtp->var_f = dtx->var_f + dty->var_f;
   dtp->var_g = dtx->var_g && dty->var_g;
   dtp->var_h = dtx->var_h + dty->var_h;

   return(dtp);
}
