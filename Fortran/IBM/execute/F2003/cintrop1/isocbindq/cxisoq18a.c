
/*
	C code for testcase "fxisoq18a.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

struct dts0 {
   long double _Complex a[5];
};

struct dts1 {
   long double _Complex a[5];
   struct dts0 d0;
};

struct dts2 {
   long double _Complex a[5];
   struct dts1 d1;
};

void initdts0(struct dts0 *);
void initdts1(struct dts1 *);
void initdts2(struct dts2 *);

int main() {

   int fnt1(struct dts0 *);
   int fnt2(struct dts0);
   int fnt3(struct dts1 *);
   int fnt4(struct dts1);
   int fnt5(struct dts2 *);
   int fnt6(struct dts2);
   int fnt7(struct dts0 *);
   int fnt7a(const struct dts0 *);
   int fnt8(struct dts0);
   int fnt8a(const struct dts0);
   int fnt9(struct dts1 *);
   int fnt9a(const struct dts1 *);
   int fnt10(struct dts1);
   int fnt10a(const struct dts1);
   int fnt11(struct dts2 *);
   int fnt11a(const struct dts2 *);
   int fnt12(struct dts2);
   int fnt12a(const struct dts2);
   int fnt13(struct dts0 *);
   int fnt14(struct dts1 *);
   int fnt15(struct dts2 *);

   struct dts0 dta;
   struct dts1 dtb;
   struct dts2 dtc;
   int i, ret;

/* Test 1 */

   initdts0(&dta);

   ret = fnt1(&dta);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dta.a[i] != (long double)(i+2)+I*(long double)(i+2) ) exit(21);
#else
      if ( dta.a[i] != createcomplexl((long double)(i+2),(long double)(i+2)) ) exit(23);
#endif
   }


/* Test 2 */

   initdts0(&dta);

   ret = fnt2(dta);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dta.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(25);
#else
      if ( dta.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(27);
#endif
   }


/* Test 3 */

   initdts1(&dtb);

   ret = fnt3(&dtb);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtb.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(29);
#else
      if ( dtb.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(31);
#endif
#ifdef CMPLX
      if ( dtb.d0.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(33);
#else
      if ( dtb.d0.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(35);
#endif
   }


/* Test 4 */

   initdts1(&dtb);

   ret = fnt4(dtb);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtb.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(37);
#else
      if ( dtb.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(39);
#endif
#ifdef CMPLX
      if ( dtb.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(41);
#else
      if ( dtb.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(43);
#endif
   }


/* Test 5 */

   initdts2(&dtc);

   ret = fnt5(&dtc);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtc.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(45);
#else
      if ( dtc.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(47);
#endif
#ifdef CMPLX
      if ( dtc.d1.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(49);
#else
      if ( dtc.d1.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(51);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(53);
#else
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(55);
#endif
   }


/* Test 6 */

   initdts2(&dtc);

   ret = fnt6(dtc);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtc.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(57);
#else
      if ( dtc.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(59);
#endif
#ifdef CMPLX
      if ( dtc.d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(61);
#else
      if ( dtc.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(63);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(65);
#else
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(67);
#endif
   }


/* Test 7 */

   initdts0(&dta);

   ret = fnt7(&dta);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dta.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(69);
#else
      if ( dta.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(71);
#endif
   }


/* Test 8 */

   initdts0(&dta);

   ret = fnt8(dta);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dta.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(73);
#else
      if ( dta.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(75);
#endif
   }


/* Test 9 */

   initdts1(&dtb);

   ret = fnt9(&dtb);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtb.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(77);
#else
      if ( dtb.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(79);
#endif
#ifdef CMPLX
      if ( dtb.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(81);
#else
      if ( dtb.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(83);
#endif
   }


/* Test 10 */

   initdts1(&dtb);

   ret = fnt10(dtb);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtb.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(85);
#else
      if ( dtb.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(87);
#endif
#ifdef CMPLX
      if ( dtb.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(89);
#else
      if ( dtb.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(91);
#endif
   }


/* Test 11 */

   initdts2(&dtc);

   ret = fnt11(&dtc);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtc.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(93);
#else
      if ( dtc.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(95);
#endif
#ifdef CMPLX
      if ( dtc.d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(97);
#else
      if ( dtc.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(99);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(101);
#else
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(103);
#endif
   }


/* Test 12 */

   initdts2(&dtc);

   ret = fnt12(dtc);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtc.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(105);
#else
      if ( dtc.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(107);
#endif
#ifdef CMPLX
      if ( dtc.d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(109);
#else
      if ( dtc.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(111);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(113);
#else
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(115);
#endif
   }


/* Test 13 */

   initdts0(&dta);

   ret = fnt13(&dta);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dta.a[i] != (long double)(i+2)+I*(long double)(i+2) ) exit(117);
#else
      if ( dta.a[i] != createcomplexl((long double)(i+2),(long double)(i+2)) ) exit(119);
#endif
   }


/* Test 14 */

   initdts1(&dtb);

   ret = fnt14(&dtb);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtb.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(121);
#else
      if ( dtb.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(123);
#endif
#ifdef CMPLX
      if ( dtb.d0.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(125);
#else
      if ( dtb.d0.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(127);
#endif
   }


/* Test 15 */

   initdts2(&dtc);

   ret = fnt15(&dtc);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtc.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(129);
#else
      if ( dtc.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(131);
#endif
#ifdef CMPLX
      if ( dtc.d1.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(133);
#else
      if ( dtc.d1.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(135);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(137);
#else
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(141);
#endif
   }


   return 0;
}

void initdts0(struct dts0 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      x->a[i] = (long double)(i+1)+I*(long double)(i+1);
#else
      x->a[i] = createcomplexl((long double)(i+1),(long double)(i+1));
#endif
   }

}

void initdts1(struct dts1 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      x->a[i] = (long double)(i+1)+I*(long double)(i+1);
#else
      x->a[i] = createcomplexl((long double)(i+1),(long double)(i+1));
#endif
   }


   initdts0(&x->d0);
}

void initdts2(struct dts2 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      x->a[i] = (long double)(i+1)+I*(long double)(i+1);
#else
      x->a[i] = createcomplexl((long double)(i+1),(long double)(i+1));
#endif
   }


   initdts1(&x->d1);
}
