
/*
	C code for testcase "fxisoi18a.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

struct dts0 {
   int_fast16_t a[5];
};

struct dts1 {
   int_fast16_t a[5];
   struct dts0 d0;
};

struct dts2 {
   int_fast16_t a[5];
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

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+2 ) exit(21);

/* Test 2 */

   initdts0(&dta);

   sub2(dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+1 ) exit(23);

/* Test 3 */

   initdts1(&dtb);

   sub3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (i+1)*2 ) exit(25);
      if ( dtb.d0.a[i] != (i+1)*2 ) exit(27);
   }


/* Test 4 */

   initdts1(&dtb);

   sub4(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != i+1 ) exit(29);
      if ( dtb.d0.a[i] != i+1 ) exit(31);
   }


/* Test 5 */

   initdts2(&dtc);

   sub5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (i+1)*2 ) exit(33);
      if ( dtc.d1.a[i] != (i+1)*2 ) exit(35);
      if ( dtc.d1.d0.a[i] != (i+1)*2 ) exit(37);
   }


/* Test 6 */

   initdts2(&dtc);

   sub6(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != i+1 ) exit(39);
      if ( dtc.d1.a[i] != i+1 ) exit(41);
      if ( dtc.d1.d0.a[i] != i+1 ) exit(43);
   }


/* Test 7 */

   initdts0(&dta);

   sub7(&dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+1 ) exit(45);

/* Test 7a */

   initdts0(&dta);

   sub7a(&dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+1 ) exit(47);

/* Test 8 */

   initdts0(&dta);

   sub8(dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+1 ) exit(49);

/* Test 8a */

   initdts0(&dta);

   sub8a(dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+1 ) exit(51);

/* Test 9 */

   initdts1(&dtb);

   sub9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != i+1 ) exit(53);
      if ( dtb.d0.a[i] != i+1 ) exit(55);
   }


/* Test 9a */

   initdts1(&dtb);

   sub9a(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != i+1 ) exit(57);
      if ( dtb.d0.a[i] != i+1 ) exit(59);
   }


/* Test 10 */

   initdts1(&dtb);

   sub10(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != i+1 ) exit(61);
      if ( dtb.d0.a[i] != i+1 ) exit(63);
   }


/* Test 10a */

   initdts1(&dtb);

   sub10a(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != i+1 ) exit(65);
      if ( dtb.d0.a[i] != i+1 ) exit(67);
   }


/* Test 11 */

   initdts2(&dtc);

   sub11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != i+1 ) exit(69);
      if ( dtc.d1.a[i] != i+1 ) exit(71);
      if ( dtc.d1.d0.a[i] != i+1 ) exit(73);
   }


/* Test 11a */

   initdts2(&dtc);

   sub11a(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != i+1 ) exit(75);
      if ( dtc.d1.a[i] != i+1 ) exit(77);
      if ( dtc.d1.d0.a[i] != i+1 ) exit(79);
   }


/* Test 12 */

   initdts2(&dtc);

   sub12(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != i+1 ) exit(81);
      if ( dtc.d1.a[i] != i+1 ) exit(83);
      if ( dtc.d1.d0.a[i] != i+1 ) exit(85);
   }


/* Test 12a */

   initdts2(&dtc);

   sub12a(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != i+1 ) exit(87);
      if ( dtc.d1.a[i] != i+1 ) exit(89);
      if ( dtc.d1.d0.a[i] != i+1 ) exit(91);
   }


/* Test 13 */

   initdts0(&dta);

   sub13(&dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+2 ) exit(93);

/* Test 14 */

   initdts1(&dtb);

   sub14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (i+1)*2 ) exit(95);
      if ( dtb.d0.a[i] != (i+1)*2 ) exit(97);
   }


/* Test 15 */

   initdts2(&dtc);

   sub15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (i+1)*2 ) exit(99);
      if ( dtc.d1.a[i] != (i+1)*2 ) exit(101);
      if ( dtc.d1.d0.a[i] != (i+1)*2 ) exit(103);
   }


   return 0;
}

void initdts0(struct dts0 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) x->a[i] = i+1;
}

void initdts1(struct dts1 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) x->a[i] = i+1;

   initdts0(&x->d0);
}

void initdts2(struct dts2 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) x->a[i] = i+1;

   initdts1(&x->d1);
}
