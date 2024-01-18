
#include <stdio.h>
#include <stdlib.h>

   #include "cmplx.h"

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

DT1 *stsum(DT1 *dtx, DT1 *dty);

int main() {

   void sub1(DT1 *dt);
   void sub2(DT1 dt);
   void sub3(DT1 *dtx, DT1 *dty);

   DT1 dt0 = {createcomplexf(2.0f,2.0f),4,createcomplexl(6.0l,6.0l),createcomplex(8.0,8.0),createcomplexf(10.0f,10.0f),12,createcomplex(14.0,14.0),16};
   DT1 dta, dtb, *dtp;

/* Test 1 */

   dta = dt0;

   sub1(&dta);

   if ( dta.var_a != createcomplexf(3.0f,3.0f) ) exit(21);
   if ( dta.var_b != 5 ) exit(23);
   if ( dta.var_c != createcomplexl(7.0l,7.0l) ) exit(25);
   if ( dta.var_d != createcomplex(9.0,9.0) ) exit(27);
   if ( dta.var_e != createcomplexf(11.0f,11.0f) ) exit(29);
   if ( dta.var_f != 13 ) exit(31);
   if ( dta.var_g != createcomplex(15.0,15.0) ) exit(33);
   if ( dta.var_h != 17 ) exit(35);

/* Test 2 */

   dta = dt0;

   sub2(dta);

   if ( dta.var_a != createcomplexf(2.0f,2.0f) ) exit(37);
   if ( dta.var_b != 4 ) exit(39);
   if ( dta.var_c != createcomplexl(6.0l,6.0l) ) exit(41);
   if ( dta.var_d != createcomplex(8.0,8.0) ) exit(43);
   if ( dta.var_e != createcomplexf(10.0f,10.0f) ) exit(45);
   if ( dta.var_f != 12 ) exit(47);
   if ( dta.var_g != createcomplex(14.0,14.0) ) exit(49);
   if ( dta.var_h != 16 ) exit(51);

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT1));
   dtp = stsum(&dta,&dta);

   sub3(dtp,&dtb);

   if ( dtb.var_a != createcomplexf(5.0f,5.0f) ) exit(53);
   if ( dtb.var_b != 9 ) exit(55);
   if ( dtb.var_c != createcomplexl(13.0l,13.0l) ) exit(57);
   if ( dtb.var_d != createcomplex(17.0,17.0) ) exit(59);
   if ( dtb.var_e != createcomplexf(21.0f,21.0f) ) exit(61);
   if ( dtb.var_f != 25 ) exit(63);
   if ( dtb.var_g != createcomplex(29.0,29.0) ) exit(65);
   if ( dtb.var_h != 33 ) exit(67);

   return 0;
}

DT1 *stsum(DT1 *dtx, DT1 *dty) {
   DT1 *dtp;

   dtp = malloc(sizeof(DT1));

   dtp->var_a = dtx->var_a + dty->var_a;
   dtp->var_b = dtx->var_b + dty->var_b;
   dtp->var_c = dtx->var_c + dty->var_c;
   dtp->var_d = dtx->var_d + dty->var_d;
   dtp->var_e = dtx->var_e + dty->var_e;
   dtp->var_f = dtx->var_f + dty->var_f;
   dtp->var_g = dtx->var_g + dty->var_g;
   dtp->var_h = dtx->var_h + dty->var_h;

   return(dtp);
}
