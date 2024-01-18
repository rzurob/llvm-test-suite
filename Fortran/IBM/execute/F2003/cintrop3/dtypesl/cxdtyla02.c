
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

DT1 *stsum(DT1 *dtx, DT1 *dty);

int main() {

   void sub1(DT1 *dt);
   void sub2(DT1 dt);
   void sub3(DT1 *dtx, DT1 *dty);

   DT1 dt0 = {2.0f,0,6.0l,8.0,10.0f,0,14.0,'A'};
   DT1 dta, dtb, *dtp;


   dta = dt0;

   sub1(&dta);

   if ( dta.var_a != 3.0f ) exit(21);
   if ( dta.var_b != 1 ) exit(23);
   if ( dta.var_c != 7.0l ) exit(25);
   if ( dta.var_d != 9.0 ) exit(27);
   if ( dta.var_e != 11.0f ) exit(29);
   if ( dta.var_f != 1 ) exit(31);
   if ( dta.var_g != 15.0 ) exit(33);
   if ( dta.var_h != 'B' ) exit(35);


   dta = dt0;

   sub2(dta);

   if ( dta.var_a != 2.0f ) exit(37);
   if ( dta.var_b != 0 ) exit(39);
   if ( dta.var_c != 6.0l ) exit(41);
   if ( dta.var_d != 8.0 ) exit(43);
   if ( dta.var_e != 10.0f ) exit(45);
   if ( dta.var_f != 0 ) exit(47);
   if ( dta.var_g != 14.0 ) exit(49);
   if ( dta.var_h != 'A' ) exit(51);


   dta = dt0;
   dtp = malloc(sizeof(DT1));
   dtp = stsum(&dta,&dta);

   sub3(dtp,&dtb);

   if ( dtb.var_a != 5.0f ) exit(53);
   if ( dtb.var_b != 1 ) exit(55);
   if ( dtb.var_c != 13.0l ) exit(57);
   if ( dtb.var_d != 17.0 ) exit(59);
   if ( dtb.var_e != 21.0f ) exit(61);
   if ( dtb.var_f != 1 ) exit(63);
   if ( dtb.var_g != 29.0 ) exit(65);
   if ( dtb.var_h != 'B' ) exit(67);

   return 0;
}

DT1 *stsum(DT1 *dtx, DT1 *dty) {
   DT1 *dtp;

   dtp = malloc(sizeof(DT1));

   dtp->var_a = dtx->var_a + dty->var_a;
   dtp->var_b = dtx->var_b && dty->var_b;
   dtp->var_c = dtx->var_c + dty->var_c;
   dtp->var_d = dtx->var_d + dty->var_d;
   dtp->var_e = dtx->var_e + dty->var_e;
   dtp->var_f = dtx->var_f && dty->var_f;
   dtp->var_g = dtx->var_g + dty->var_g;
   dtp->var_h = dtx->var_h;

   return(dtp);
}
