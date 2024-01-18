
/*
C code for testcase "fxison18a.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

struct dts0 {
   float _Complex a[5];
   double _Complex b[3];
};

struct dts1 {
   float _Complex a[5];
   double _Complex b[3];
   struct dts0 d0;
};

struct dts2 {
   float _Complex a[5];
   double _Complex b[3];
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
      if ( dta.a[i] != (float)(i+2)+I*(float)(i+2) ) exit(21);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != (double)(i+2)+I*(double)(i+2) ) exit(25);
   }


/* Test 2 */

   initdts0(&dta);

   sub2(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(29);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(33);
   }


/* Test 3 */

   initdts1(&dtb);

   sub3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(37);
      if ( dtb.d0.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(41);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(45);
      if ( dtb.d0.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(49);
   }

/* Test 4 */

   initdts1(&dtb);

   sub4(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(53);
      if ( dtb.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(57);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(61);
      if ( dtb.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(65);
   }

/* Test 5 */

   initdts2(&dtc);

   sub5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(69);
      if ( dtc.d1.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(73);
      if ( dtc.d1.d0.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(77);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(81);
      if ( dtc.d1.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(85);
      if ( dtc.d1.d0.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(89);
   }

/* Test 6 */

   initdts2(&dtc);

   sub6(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(93);
      if ( dtc.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(97);
      if ( dtc.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(101);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(105);
      if ( dtc.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(109);
      if ( dtc.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(113);
   }

/* Test 7 */

   initdts0(&dta);

   sub7(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(117);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(121);
   }


/* Test 7a */

   initdts0(&dta);

   sub7a(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(125);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(129);
   }


/* Test 8 */

   initdts0(&dta);

   sub8(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(133);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(137);
   }


/* Test 8a */

   initdts0(&dta);

   sub8a(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(143);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(147);
   }


/* Test 9 */

   initdts1(&dtb);

   sub9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(151);
      if ( dtb.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(155);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(159);
      if ( dtb.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(163);
   }

/* Test 9a */

   initdts1(&dtb);

   sub9a(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(167);
      if ( dtb.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(171);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(175);
      if ( dtb.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(179);
   }

/* Test 10 */

   initdts1(&dtb);

   sub10(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(183);
      if ( dtb.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(187);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(191);
      if ( dtb.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(195);
   }

/* Test 10a */

   initdts1(&dtb);

   sub10a(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(199);
      if ( dtb.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(203);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(207);
      if ( dtb.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(211);
   }

/* Test 11 */

   initdts2(&dtc);

   sub11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(215);
      if ( dtc.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(219);
      if ( dtc.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(223);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(227);
      if ( dtc.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(231);
      if ( dtc.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(235);
   }

/* Test 11a */

   initdts2(&dtc);

   sub11a(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(239);
      if ( dtc.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(243);
      if ( dtc.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(247);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(253);
      if ( dtc.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(257);
      if ( dtc.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(261);
   }

/* Test 12 */

   initdts2(&dtc);

   sub12(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(265);
      if ( dtc.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(269);
      if ( dtc.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(273);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(277);
      if ( dtc.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(281);
      if ( dtc.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(285);
   }

/* Test 12a */

   initdts2(&dtc);

   sub12a(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(289);
      if ( dtc.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(293);
      if ( dtc.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(297);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(301);
      if ( dtc.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(305);
      if ( dtc.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(309);
   }

/* Test 13 */

   initdts0(&dta);

   sub13(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (float)(i+2)+I*(float)(i+2) ) exit(313);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != (double)(i+2)+I*(double)(i+2) ) exit(317);
   }


/* Test 14 */

   initdts1(&dtb);

   sub14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(321);
      if ( dtb.d0.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(325);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(329);
      if ( dtb.d0.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(333);
   }

/* Test 15 */

   initdts2(&dtc);

   sub15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(337);
      if ( dtc.d1.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(341);
      if ( dtc.d1.d0.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(345);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(349);
      if ( dtc.d1.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(353);
      if ( dtc.d1.d0.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(357);
   }

   return 0;
}

void initdts0(struct dts0 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      x->b[i] = (double)(i+1)+I*(double)(i+1);
   }

}

void initdts1(struct dts1 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      x->b[i] = (double)(i+1)+I*(double)(i+1);
   }


   initdts0(&x->d0);
}

void initdts2(struct dts2 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      x->b[i] = (double)(i+1)+I*(double)(i+1);
   }


   initdts1(&x->d1);
}
