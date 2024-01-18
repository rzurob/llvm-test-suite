
#include <stdio.h>
#include <stdlib.h>

#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

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

int main () {

   DT1 *fun1(DT1 *);
   DT1 *fun2(DT1);
   DT1 *fun3(DT1 *, DT1 *);
   DT1 *fun4(DT1 *, DT1 *);

#ifdef CMPLX
   DT1 dt0 = {2.0f+I*2.0f,4,6.0l+I*6.0l,8.0+I*8.0,10.0f+I*10.0f,12,14.0+I*14.0,16};
#else
   DT1 dt0 = {createcomplexf(2.0f,2.0f),4,createcomplexl(6.0l,6.0l),createcomplex(8.0,8.0),createcomplexf(10.0f,10.0f),12,createcomplex(14.0,14.0),16};
#endif
   DT1 dta, *dtb, *dtc, *dtp;

/* Test 1 */

   dta = dt0;

   dtb = fun1(&dta);

#ifdef CMPLX
   if ( dta.var_a != 3.0f+I*3.0f || dtb->var_a != 3.0f+I*3.0f ) exit(21);
#else
   if ( dta.var_a != createcomplexf(3.0f,3.0f) || dtb->var_a != createcomplexf(3.0f,3.0f) ) exit(21);
#endif
   if ( dta.var_b != 5 || dtb->var_b != 5 ) exit(23);
#ifdef CMPLX
   if ( dta.var_c != 7.0l+I*7.0l || dtb->var_c != 7.0l+I*7.0l ) exit(25);
#else
   if ( dta.var_c != createcomplexl(7.0l,7.0l) || dtb->var_c != createcomplexl(7.0l,7.0l) ) exit(25);
#endif
#ifdef CMPLX
   if ( dta.var_d != 9.0+I*9.0 || dtb->var_d != 9.0+I*9.0 ) exit(27);
#else
   if ( dta.var_d != createcomplex(9.0,9.0) || dtb->var_d != createcomplex(9.0,9.0) ) exit(27);
#endif
#ifdef CMPLX
   if ( dta.var_e != 11.0f+I*11.0f || dtb->var_e != 11.0f+I*11.0f ) exit(29);
#else
   if ( dta.var_e != createcomplexf(11.0f,11.0f) || dtb->var_e != createcomplexf(11.0f,11.0f) ) exit(29);
#endif
   if ( dta.var_f != 13 || dtb->var_f != 13 ) exit(31);
#ifdef CMPLX
   if ( dta.var_g != 15.0+I*15.0 || dtb->var_g != 15.0+I*15.0 ) exit(33);
#else
   if ( dta.var_g != createcomplex(15.0,15.0) || dtb->var_g != createcomplex(15.0,15.0) ) exit(33);
#endif
   if ( dta.var_h != 17 || dtb->var_h != 17 ) exit(35);

/* Test 2 */

   dta = dt0;

   dtb = fun2(dta);

#ifdef CMPLX
   if ( dta.var_a != 2.0f+I*2.0f || dtb->var_a != 3.0f+I*3.0f ) exit(37);
#else
   if ( dta.var_a != createcomplexf(2.0f,2.0f) || dtb->var_a != createcomplexf(3.0f,3.0f) ) exit(37);
#endif
   if ( dta.var_b != 4 || dtb->var_b != 5 ) exit(39);
#ifdef CMPLX
   if ( dta.var_c != 6.0l+I*6.0l || dtb->var_c != 7.0l+I*7.0l ) exit(41);
#else
   if ( dta.var_c != createcomplexl(6.0l,6.0l) || dtb->var_c != createcomplexl(7.0l,7.0l) ) exit(41);
#endif
#ifdef CMPLX
   if ( dta.var_d != 8.0+I*8.0 || dtb->var_d != 9.0+I*9.0 ) exit(43);
#else
   if ( dta.var_d != createcomplex(8.0,8.0) || dtb->var_d != createcomplex(9.0,9.0) ) exit(43);
#endif
#ifdef CMPLX
   if ( dta.var_e != 10.0f+I*10.0f || dtb->var_e != 11.0f+I*11.0f ) exit(45);
#else
   if ( dta.var_e != createcomplexf(10.0f,10.0f) || dtb->var_e != createcomplexf(11.0f,11.0f) ) exit(45);
#endif
   if ( dta.var_f != 12 || dtb->var_f != 13 ) exit(47);
#ifdef CMPLX
   if ( dta.var_g != 14.0+I*14.0 || dtb->var_g != 15.0+I*15.0 ) exit(49);
#else
   if ( dta.var_g != createcomplex(14.0,14.0) || dtb->var_g != createcomplex(15.0,15.0) ) exit(49);
#endif
   if ( dta.var_h != 16 || dtb->var_h != 17 ) exit(51);

/* Test 3 */

   dta = dt0;
   dtp = malloc(sizeof(DT1));
   dtb = malloc(sizeof(DT1));
   dtp = stsum(&dta,&dta);

   dtc = fun3(dtp,dtb);

#ifdef CMPLX
   if ( dtb->var_a != 5.0f+I*5.0f || dtc->var_a != 5.0f+I*5.0f ) exit(53);
#else
   if ( dtb->var_a != createcomplexf(5.0f,5.0f) || dtc->var_a != createcomplexf(5.0f,5.0f) ) exit(53);
#endif
   if ( dtb->var_b != 9 || dtc->var_b != 9 ) exit(55);
#ifdef CMPLX
   if ( dtb->var_c != 13.0l+I*13.0l || dtc->var_c != 13.0l+I*13.0l ) exit(57);
#else
   if ( dtb->var_c != createcomplexl(13.0l,13.0l) || dtc->var_c != createcomplexl(13.0l,13.0l) ) exit(57);
#endif
#ifdef CMPLX
   if ( dtb->var_d != 17.0+I*17.0 || dtc->var_d != 17.0+I*17.0 ) exit(59);
#else
   if ( dtb->var_d != createcomplex(17.0,17.0) || dtc->var_d != createcomplex(17.0,17.0) ) exit(59);
#endif
#ifdef CMPLX
   if ( dtb->var_e != 21.0f+I*21.0f || dtc->var_e != 21.0f+I*21.0f ) exit(61);
#else
   if ( dtb->var_e != createcomplexf(21.0f,21.0f) || dtc->var_e != createcomplexf(21.0f,21.0f) ) exit(61);
#endif
   if ( dtb->var_f != 25 || dtc->var_f != 25 ) exit(63);
#ifdef CMPLX
   if ( dtb->var_g != 29.0+I*29.0 || dtc->var_g != 29.0+I*29.0 ) exit(65);
#else
   if ( dtb->var_g != createcomplex(29.0,29.0) || dtc->var_g != createcomplex(29.0,29.0) ) exit(65);
#endif
   if ( dtb->var_h != 33 || dtc->var_h != 33 ) exit(67);

/* Test 4 */

   dta = dt0;
   dtp = malloc(sizeof(DT1));
   dtb = malloc(sizeof(DT1));
   dtp = stsum(&dta,&dta);

   dtc = fun4(dtp,dtb);

#ifdef CMPLX
   if ( dtb->var_a != 5.0f+I*5.0f || dtc->var_a != 6.0f+I*6.0f ) exit(69);
#else
   if ( dtb->var_a != createcomplexf(5.0f,5.0f) || dtc->var_a != createcomplexf(6.0f,6.0f) ) exit(69);
#endif
   if ( dtb->var_b != 9 || dtc->var_b != 10 ) exit(71);
#ifdef CMPLX
   if ( dtb->var_c != 13.0l+I*13.0l || dtc->var_c != 14.0l+I*14.0l ) exit(73);
#else
   if ( dtb->var_c != createcomplexl(13.0l,13.0l) || dtc->var_c != createcomplexl(14.0l,14.0l) ) exit(73);
#endif
#ifdef CMPLX
   if ( dtb->var_d != 17.0+I*17.0 || dtc->var_d != 18.0+I*18.0 ) exit(75);
#else
   if ( dtb->var_d != createcomplex(17.0,17.0) || dtc->var_d != createcomplex(18.0,18.0) ) exit(75);
#endif
#ifdef CMPLX
   if ( dtb->var_e != 21.0f+I*21.0f || dtc->var_e != 22.0f+I*22.0f ) exit(77);
#else
   if ( dtb->var_e != createcomplexf(21.0f,21.0f) || dtc->var_e != createcomplexf(22.0f,22.0f) ) exit(77);
#endif
   if ( dtb->var_f != 25 || dtc->var_f != 26 ) exit(79);
#ifdef CMPLX
   if ( dtb->var_g != 29.0+I*29.0 || dtc->var_g != 30.0+I*30.0 ) exit(81);
#else
   if ( dtb->var_g != createcomplex(29.0,29.0) || dtc->var_g != createcomplex(30.0,30.0) ) exit(81);
#endif
   if ( dtb->var_h != 33 || dtc->var_h != 34 ) exit(83);

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

