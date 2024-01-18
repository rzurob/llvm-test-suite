
/*
C code for testcase "fxisoo18a.f"
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

   void sub1(struct dts0 *);
   void sub2(struct dts0);
   void sub3(struct dts1 *);
   void sub4(struct dts1);
   void sub5(struct dts2 *);
   void sub6(struct dts2);
   void sub7(struct dts0 *);
   void sub7a(const struct dts0 *);
   void sub8(struct dts0);
   void sub8a(const struct dts0);
   void sub9(struct dts1 *);
   void sub9a(const struct dts1 *);
   void sub10(struct dts1);
   void sub10a(const struct dts1);
   void sub11(struct dts2 *);
   void sub11a(const struct dts2 *);
   void sub12(struct dts2);
   void sub12a(const struct dts2);
   void sub13(struct dts0 *);
   void sub14(struct dts1 *);
   void sub15(struct dts2 *);

   struct dts0 dta;
   struct dts1 dtb;
   struct dts2 dtc;
   int i, ret;

/* Test 1 */

   initdts0(&dta);

   sub1(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexl((long double)(i+2),(long double)(i+2)) ) exit(23);
   }


/* Test 2 */

   initdts0(&dta);

   sub2(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(27);
   }


/* Test 3 */

   initdts1(&dtb);

   sub3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(31);
      if ( dtb.d0.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(35);
   }


/* Test 4 */

   initdts1(&dtb);

   sub4(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(39);
      if ( dtb.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(43);
   }


/* Test 5 */

   initdts2(&dtc);

   sub5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(47);
      if ( dtc.d1.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(51);
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(55);
   }


/* Test 6 */

   initdts2(&dtc);

   sub6(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(59);
      if ( dtc.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(63);
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(67);
   }


/* Test 7 */

   initdts0(&dta);

   sub7(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(71);
   }


/* Test 7a */

   initdts0(&dta);

   sub7a(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(75);
   }


/* Test 8 */

   initdts0(&dta);

   sub8(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(79);
   }


/* Test 8a */

   initdts0(&dta);

   sub8a(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(83);
   }


/* Test 9 */

   initdts1(&dtb);

   sub9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(87);
      if ( dtb.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(91);
   }


/* Test 9a */

   initdts1(&dtb);

   sub9a(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(95);
      if ( dtb.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(99);
   }


/* Test 10 */

   initdts1(&dtb);

   sub10(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(103);
      if ( dtb.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(107);
   }


/* Test 10a */

   initdts1(&dtb);

   sub10a(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(111);
      if ( dtb.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(115);
   }


/* Test 11 */

   initdts2(&dtc);

   sub11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(119);
      if ( dtc.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(123);
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(127);
   }


/* Test 11a */

   initdts2(&dtc);

   sub11a(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(131);
      if ( dtc.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(135);
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(141);
   }


/* Test 12 */

   initdts2(&dtc);

   sub12(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(145);
      if ( dtc.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(149);
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(153);
   }


/* Test 12a */

   initdts2(&dtc);

   sub12a(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(157);
      if ( dtc.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(161);
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(165);
   }


/* Test 13 */

   initdts0(&dta);

   sub13(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexl((long double)(i+2),(long double)(i+2)) ) exit(169);
   }


/* Test 14 */

   initdts1(&dtb);

   sub14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(173);
      if ( dtb.d0.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(177);
   }


/* Test 15 */

   initdts2(&dtc);

   sub15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(181);
      if ( dtc.d1.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(185);
      if ( dtc.d1.d0.a[i] != createcomplexl((long double)((i+1)*2),(long double)((i+1)*2)) ) exit(189);
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
