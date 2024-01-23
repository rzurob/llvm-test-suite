
/*
C code for testcase "fxisoq18a.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

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
      if ( dta.a[i] != createcomplexl((long double)(i+2),(long double)(i+2)) ) exit(23);
   }


/* Test 2 */

   initdts0(&dta);

   ret = fnt2(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(27);
   }


/* Test 3 */

   initdts1(&dtb);

   ret = fnt3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(31);
      if ( dtb.d0.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(35);
   }


/* Test 4 */

   initdts1(&dtb);

   ret = fnt4(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(39);
      if ( dtb.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(43);
   }


/* Test 5 */

   initdts2(&dtc);

   ret = fnt5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(47);
      if ( dtc.d1.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(51);
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(55);
   }


/* Test 6 */

   initdts2(&dtc);

   ret = fnt6(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(59);
      if ( dtc.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(63);
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(67);
   }


/* Test 7 */

   initdts0(&dta);

   ret = fnt7(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(71);
   }


/* Test 8 */

   initdts0(&dta);

   ret = fnt8(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(75);
   }


/* Test 9 */

   initdts1(&dtb);

   ret = fnt9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(79);
      if ( dtb.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(83);
   }


/* Test 10 */

   initdts1(&dtb);

   ret = fnt10(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(87);
      if ( dtb.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(91);
   }


/* Test 11 */

   initdts2(&dtc);

   ret = fnt11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(95);
      if ( dtc.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(99);
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(103);
   }


/* Test 12 */

   initdts2(&dtc);

   ret = fnt12(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(107);
      if ( dtc.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(111);
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(115);
   }


/* Test 13 */

   initdts0(&dta);

   ret = fnt13(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexl((long double)(i+2),(long double)(i+2)) ) exit(119);
   }


/* Test 14 */

   initdts1(&dtb);

   ret = fnt14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(123);
      if ( dtb.d0.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(127);
   }


/* Test 15 */

   initdts2(&dtc);

   ret = fnt15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(131);
      if ( dtc.d1.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(135);
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(141);
   }


   return 0;
}

void initdts0(struct dts0 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = createcomplexl((long double)(i+1),(long double)(i+1));
   }

}

void initdts1(struct dts1 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = createcomplexl((long double)(i+1),(long double)(i+1));
   }


   initdts0(&x->d0);
}

void initdts2(struct dts2 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = createcomplexl((long double)(i+1),(long double)(i+1));
   }


   initdts1(&x->d1);
}
