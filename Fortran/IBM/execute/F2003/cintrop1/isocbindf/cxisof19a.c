
/*
	C code for testcase "fxisof18a.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

struct dts0 {
   int_least32_t a[5];
   int_least64_t b[3];
};

struct dts1 {
   int_least32_t a[5];
   int_least64_t b[3];
   struct dts0 d0;
};

struct dts2 {
   int_least32_t a[5];
   int_least64_t b[3];
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
   for ( i = 0; i < 3; i++ ) if ( dta.b[i] != i+2 ) exit(23);

/* Test 2 */

   initdts0(&dta);

   sub2(dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+1 ) exit(25);
   for ( i = 0; i < 3; i++ ) if ( dta.b[i] != i+1 ) exit(27);

/* Test 3 */

   initdts1(&dtb);

   sub3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (i+1)*2 ) exit(29);
      if ( dtb.d0.a[i] != (i+1)*2 ) exit(31);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (i+1)*2 ) exit(33);
      if ( dtb.d0.b[i] != (i+1)*2 ) exit(35);
   }

/* Test 4 */

   initdts1(&dtb);

   sub4(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != i+1 ) exit(37);
      if ( dtb.d0.a[i] != i+1 ) exit(39);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != i+1 ) exit(41);
      if ( dtb.d0.b[i] != i+1 ) exit(43);
   }

/* Test 5 */

   initdts2(&dtc);

   sub5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (i+1)*2 ) exit(45);
      if ( dtc.d1.a[i] != (i+1)*2 ) exit(47);
      if ( dtc.d1.d0.a[i] != (i+1)*2 ) exit(49);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (i+1)*2 ) exit(51);
      if ( dtc.d1.b[i] != (i+1)*2 ) exit(53);
      if ( dtc.d1.d0.b[i] != (i+1)*2 ) exit(55);
   }

/* Test 6 */

   initdts2(&dtc);

   sub6(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != i+1 ) exit(57);
      if ( dtc.d1.a[i] != i+1 ) exit(59);
      if ( dtc.d1.d0.a[i] != i+1 ) exit(61);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != i+1 ) exit(63);
      if ( dtc.d1.b[i] != i+1 ) exit(65);
      if ( dtc.d1.d0.b[i] != i+1 ) exit(67);
   }

/* Test 7 */

   initdts0(&dta);

   sub7(&dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+1 ) exit(69);
   for ( i = 0; i < 3; i++ ) if ( dta.b[i] != i+1 ) exit(71);

/* Test 7a */

   initdts0(&dta);

   sub7a(&dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+1 ) exit(73);
   for ( i = 0; i < 3; i++ ) if ( dta.b[i] != i+1 ) exit(75);

/* Test 8 */

   initdts0(&dta);

   sub8(dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+1 ) exit(77);
   for ( i = 0; i < 3; i++ ) if ( dta.b[i] != i+1 ) exit(79);

/* Test 8a */

   initdts0(&dta);

   sub8a(dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+1 ) exit(81);
   for ( i = 0; i < 3; i++ ) if ( dta.b[i] != i+1 ) exit(83);

/* Test 9 */

   initdts1(&dtb);

   sub9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != i+1 ) exit(85);
      if ( dtb.d0.a[i] != i+1 ) exit(87);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != i+1 ) exit(89);
      if ( dtb.d0.b[i] != i+1 ) exit(91);
   }

/* Test 9a */

   initdts1(&dtb);

   sub9a(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != i+1 ) exit(93);
      if ( dtb.d0.a[i] != i+1 ) exit(95);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != i+1 ) exit(97);
      if ( dtb.d0.b[i] != i+1 ) exit(99);
   }

/* Test 10 */

   initdts1(&dtb);

   sub10(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != i+1 ) exit(101);
      if ( dtb.d0.a[i] != i+1 ) exit(103);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != i+1 ) exit(105);
      if ( dtb.d0.b[i] != i+1 ) exit(107);
   }

/* Test 10a */

   initdts1(&dtb);

   sub10a(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != i+1 ) exit(109);
      if ( dtb.d0.a[i] != i+1 ) exit(111);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != i+1 ) exit(113);
      if ( dtb.d0.b[i] != i+1 ) exit(115);
   }

/* Test 11 */

   initdts2(&dtc);

   sub11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != i+1 ) exit(117);
      if ( dtc.d1.a[i] != i+1 ) exit(119);
      if ( dtc.d1.d0.a[i] != i+1 ) exit(121);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != i+1 ) exit(123);
      if ( dtc.d1.b[i] != i+1 ) exit(125);
      if ( dtc.d1.d0.b[i] != i+1 ) exit(127);
   }

/* Test 11a */

   initdts2(&dtc);

   sub11a(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != i+1 ) exit(129);
      if ( dtc.d1.a[i] != i+1 ) exit(131);
      if ( dtc.d1.d0.a[i] != i+1 ) exit(133);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != i+1 ) exit(135);
      if ( dtc.d1.b[i] != i+1 ) exit(137);
      if ( dtc.d1.d0.b[i] != i+1 ) exit(141);
   }

/* Test 12 */

   initdts2(&dtc);

   sub12(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != i+1 ) exit(143);
      if ( dtc.d1.a[i] != i+1 ) exit(145);
      if ( dtc.d1.d0.a[i] != i+1 ) exit(147);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != i+1 ) exit(149);
      if ( dtc.d1.b[i] != i+1 ) exit(151);
      if ( dtc.d1.d0.b[i] != i+1 ) exit(153);
   }

/* Test 12a */

   initdts2(&dtc);

   sub12a(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != i+1 ) exit(155);
      if ( dtc.d1.a[i] != i+1 ) exit(157);
      if ( dtc.d1.d0.a[i] != i+1 ) exit(159);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != i+1 ) exit(161);
      if ( dtc.d1.b[i] != i+1 ) exit(163);
      if ( dtc.d1.d0.b[i] != i+1 ) exit(165);
   }

/* Test 13 */

   initdts0(&dta);

   sub13(&dta);

   for ( i = 0; i < 5; i++ ) if ( dta.a[i] != i+2 ) exit(167);
   for ( i = 0; i < 3; i++ ) if ( dta.b[i] != i+2 ) exit(169);

/* Test 14 */

   initdts1(&dtb);

   sub14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (i+1)*2 ) exit(171);
      if ( dtb.d0.a[i] != (i+1)*2 ) exit(173);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (i+1)*2 ) exit(175);
      if ( dtb.d0.b[i] != (i+1)*2 ) exit(177);
   }

/* Test 15 */

   initdts2(&dtc);

   sub15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (i+1)*2 ) exit(179);
      if ( dtc.d1.a[i] != (i+1)*2 ) exit(181);
      if ( dtc.d1.d0.a[i] != (i+1)*2 ) exit(183);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (i+1)*2 ) exit(185);
      if ( dtc.d1.b[i] != (i+1)*2 ) exit(187);
      if ( dtc.d1.d0.b[i] != (i+1)*2 ) exit(189);
   }

   return 0;
}

void initdts0(struct dts0 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) x->a[i] = i+1;
   for ( i = 0; i < 3; i++ ) x->b[i] = i+1;
}

void initdts1(struct dts1 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) x->a[i] = i+1;
   for ( i = 0; i < 3; i++ ) x->b[i] = i+1;

   initdts0(&x->d0);
}

void initdts2(struct dts2 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) x->a[i] = i+1;
   for ( i = 0; i < 3; i++ ) x->b[i] = i+1;

   initdts1(&x->d1);
}
