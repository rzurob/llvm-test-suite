
/*
C code for testcase "fxison12.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

struct dt0 {
   float _Complex a;
   double _Complex b;
};

struct dt1 {
   float _Complex a;
   double _Complex b;
   struct dt0 d0;
};

struct dt2 {
   float _Complex a;
   double _Complex b;
   struct dt1 d1;
};

int main() {

   void sub1(struct dt0 *);
   void sub2(struct dt0);
   void sub3(struct dt1 *);
   void sub4(struct dt1);
   void sub5(struct dt2 *);
   void sub6(struct dt2);
   void sub7(struct dt0 *);
   void sub7a(const struct dt0 *);
   void sub8(struct dt0);
   void sub8a(const struct dt0);
   void sub9(struct dt1 *);
   void sub9a(const struct dt1 *);
   void sub10(struct dt1);
   void sub10a(const struct dt1);
   void sub11(struct dt2 *);
   void sub11a(const struct dt2 *);
   void sub12(struct dt2);
   void sub12a(const struct dt2);
   void sub13(struct dt0 *);
   void sub14(struct dt1 *);
   void sub15(struct dt2 *);

   struct dt0 dta;
   struct dt1 dtb;
   struct dt2 dtc;
   int ret;

/* Test 1 */

   dta.a = 5.0f+I*5.0f;
   dta.b = 10.0+I*10.0;

   sub1(&dta);

   if ( dta.a != 10.0f+I*10.0f ) exit(21);
   if ( dta.b != 20.0+I*20.0 ) exit(25);

/* Test 2 */

   dta.a = 5.0f+I*5.0f;
   dta.b = 10.0+I*10.0;

   sub2(dta);

   if ( dta.a != 5.0f+I*5.0f ) exit(29);
   if ( dta.b != 10.0+I*10.0 ) exit(33);

/* Test 3 */

   dtb.a = 5.0f+I*5.0f;
   dtb.b = 10.0+I*10.0;
   dtb.d0.a = 5.0f+I*5.0f;
   dtb.d0.b = 10.0+I*10.0;

   sub3(&dtb);

   if ( dtb.a != 10.0f+I*10.0f ) exit(37);
   if ( dtb.b != 20.0+I*20.0 ) exit(41);
   if ( dtb.d0.a != 10.0f+I*10.0f ) exit(45);
   if ( dtb.d0.b != 20.0+I*20.0 ) exit(49);

/* Test 4 */

   dtb.a = 5.0f+I*5.0f;
   dtb.b = 10.0+I*10.0;
   dtb.d0.a = 5.0f+I*5.0f;
   dtb.d0.b = 10.0+I*10.0;

   sub4(dtb);

   if ( dtb.a != 5.0f+I*5.0f ) exit(53);
   if ( dtb.b != 10.0+I*10.0 ) exit(57);
   if ( dtb.d0.a != 5.0f+I*5.0f ) exit(61);
   if ( dtb.d0.b != 10.0+I*10.0 ) exit(65);

/* Test 5 */

   dtc.a = 5.0f+I*5.0f;
   dtc.b = 10.0+I*10.0;
   dtc.d1.a = 5.0f+I*5.0f;
   dtc.d1.b = 10.0+I*10.0;
   dtc.d1.d0.a = 5.0f+I*5.0f;
   dtc.d1.d0.b = 10.0+I*10.0;

   sub5(&dtc);

   if ( dtc.a != 10.0f+I*10.0f ) exit(69);
   if ( dtc.b != 20.0+I*20.0 ) exit(73);
   if ( dtc.d1.a != 10.0f+I*10.0f ) exit(77);
   if ( dtc.d1.b != 20.0+I*20.0 ) exit(81);
   if ( dtc.d1.d0.a != 10.0f+I*10.0f ) exit(85);
   if ( dtc.d1.d0.b != 20.0+I*20.0 ) exit(89);

/* Test 6 */

   dtc.a = 5.0f+I*5.0f;
   dtc.b = 10.0+I*10.0;
   dtc.d1.a = 5.0f+I*5.0f;
   dtc.d1.b = 10.0+I*10.0;
   dtc.d1.d0.a = 5.0f+I*5.0f;
   dtc.d1.d0.b = 10.0+I*10.0;

   sub6(dtc);

   if ( dtc.a != 5.0f+I*5.0f ) exit(93);
   if ( dtc.b != 10.0+I*10.0 ) exit(97);
   if ( dtc.d1.a != 5.0f+I*5.0f ) exit(101);
   if ( dtc.d1.b != 10.0+I*10.0 ) exit(105);
   if ( dtc.d1.d0.a != 5.0f+I*5.0f ) exit(109);
   if ( dtc.d1.d0.b != 10.0+I*10.0 ) exit(113);

/* Test 7 */

   dta.a = 5.0f+I*5.0f;
   dta.b = 10.0+I*10.0;

   sub7(&dta);

   if ( dta.a != 5.0f+I*5.0f ) exit(117);
   if ( dta.b != 10.0+I*10.0 ) exit(121);

/* Test 7a */

   dta.a = 5.0f+I*5.0f;
   dta.b = 10.0+I*10.0;

   sub7a(&dta);

   if ( dta.a != 5.0f+I*5.0f ) exit(125);
   if ( dta.b != 10.0+I*10.0 ) exit(129);

/* Test 8 */

   dta.a = 5.0f+I*5.0f;
   dta.b = 10.0+I*10.0;

   sub8(dta);

   if ( dta.a != 5.0f+I*5.0f ) exit(133);
   if ( dta.b != 10.0+I*10.0 ) exit(137);

/* Test 8a */

   dta.a = 5.0f+I*5.0f;
   dta.b = 10.0+I*10.0;

   sub8a(dta);

   if ( dta.a != 5.0f+I*5.0f ) exit(143);
   if ( dta.b != 10.0+I*10.0 ) exit(147);

/* Test 9 */

   dtb.a = 5.0f+I*5.0f;
   dtb.b = 10.0+I*10.0;
   dtb.d0.a = 5.0f+I*5.0f;
   dtb.d0.b = 10.0+I*10.0;

   sub9(&dtb);

   if ( dtb.a != 5.0f+I*5.0f ) exit(151);
   if ( dtb.b != 10.0+I*10.0 ) exit(155);
   if ( dtb.d0.a != 5.0f+I*5.0f ) exit(159);
   if ( dtb.d0.b != 10.0+I*10.0 ) exit(163);

/* Test 9a */

   dtb.a = 5.0f+I*5.0f;
   dtb.b = 10.0+I*10.0;
   dtb.d0.a = 5.0f+I*5.0f;
   dtb.d0.b = 10.0+I*10.0;

   sub9a(&dtb);

   if ( dtb.a != 5.0f+I*5.0f ) exit(167);
   if ( dtb.b != 10.0+I*10.0 ) exit(171);
   if ( dtb.d0.a != 5.0f+I*5.0f ) exit(175);
   if ( dtb.d0.b != 10.0+I*10.0 ) exit(179);

/* Test 10 */

   dtb.a = 5.0f+I*5.0f;
   dtb.b = 10.0+I*10.0;
   dtb.d0.a = 5.0f+I*5.0f;
   dtb.d0.b = 10.0+I*10.0;

   sub10(dtb);

   if ( dtb.a != 5.0f+I*5.0f ) exit(183);
   if ( dtb.b != 10.0+I*10.0 ) exit(187);
   if ( dtb.d0.a != 5.0f+I*5.0f ) exit(191);
   if ( dtb.d0.b != 10.0+I*10.0 ) exit(195);

/* Test 10a */

   dtb.a = 5.0f+I*5.0f;
   dtb.b = 10.0+I*10.0;
   dtb.d0.a = 5.0f+I*5.0f;
   dtb.d0.b = 10.0+I*10.0;

   sub10a(dtb);

   if ( dtb.a != 5.0f+I*5.0f ) exit(199);
   if ( dtb.b != 10.0+I*10.0 ) exit(203);
   if ( dtb.d0.a != 5.0f+I*5.0f ) exit(207);
   if ( dtb.d0.b != 10.0+I*10.0 ) exit(211);

/* Test 11 */

   dtc.a = 5.0f+I*5.0f;
   dtc.b = 10.0+I*10.0;
   dtc.d1.a = 5.0f+I*5.0f;
   dtc.d1.b = 10.0+I*10.0;
   dtc.d1.d0.a = 5.0f+I*5.0f;
   dtc.d1.d0.b = 10.0+I*10.0;

   sub11(&dtc);

   if ( dtc.a != 5.0f+I*5.0f ) exit(215);
   if ( dtc.b != 10.0+I*10.0 ) exit(219);
   if ( dtc.d1.a != 5.0f+I*5.0f ) exit(223);
   if ( dtc.d1.b != 10.0+I*10.0 ) exit(227);
   if ( dtc.d1.d0.a != 5.0f+I*5.0f ) exit(231);
   if ( dtc.d1.d0.b != 10.0+I*10.0 ) exit(235);

/* Test 11a */

   dtc.a = 5.0f+I*5.0f;
   dtc.b = 10.0+I*10.0;
   dtc.d1.a = 5.0f+I*5.0f;
   dtc.d1.b = 10.0+I*10.0;
   dtc.d1.d0.a = 5.0f+I*5.0f;
   dtc.d1.d0.b = 10.0+I*10.0;

   sub11a(&dtc);

   if ( dtc.a != 5.0f+I*5.0f ) exit(239);
   if ( dtc.b != 10.0+I*10.0 ) exit(243);
   if ( dtc.d1.a != 5.0f+I*5.0f ) exit(247);
   if ( dtc.d1.b != 10.0+I*10.0 ) exit(253);
   if ( dtc.d1.d0.a != 5.0f+I*5.0f ) exit(257);
   if ( dtc.d1.d0.b != 10.0+I*10.0 ) exit(261);

/* Test 12 */

   dtc.a = 5.0f+I*5.0f;
   dtc.b = 10.0+I*10.0;
   dtc.d1.a = 5.0f+I*5.0f;
   dtc.d1.b = 10.0+I*10.0;
   dtc.d1.d0.a = 5.0f+I*5.0f;
   dtc.d1.d0.b = 10.0+I*10.0;

   sub12(dtc);

   if ( dtc.a != 5.0f+I*5.0f ) exit(265);
   if ( dtc.b != 10.0+I*10.0 ) exit(269);
   if ( dtc.d1.a != 5.0f+I*5.0f ) exit(273);
   if ( dtc.d1.b != 10.0+I*10.0 ) exit(277);
   if ( dtc.d1.d0.a != 5.0f+I*5.0f ) exit(281);
   if ( dtc.d1.d0.b != 10.0+I*10.0 ) exit(285);

/* Test 12a */

   dtc.a = 5.0f+I*5.0f;
   dtc.b = 10.0+I*10.0;
   dtc.d1.a = 5.0f+I*5.0f;
   dtc.d1.b = 10.0+I*10.0;
   dtc.d1.d0.a = 5.0f+I*5.0f;
   dtc.d1.d0.b = 10.0+I*10.0;

   sub12a(dtc);

   if ( dtc.a != 5.0f+I*5.0f ) exit(289);
   if ( dtc.b != 10.0+I*10.0 ) exit(293);
   if ( dtc.d1.a != 5.0f+I*5.0f ) exit(297);
   if ( dtc.d1.b != 10.0+I*10.0 ) exit(301);
   if ( dtc.d1.d0.a != 5.0f+I*5.0f ) exit(305);
   if ( dtc.d1.d0.b != 10.0+I*10.0 ) exit(309);

/* Test 13 */

   dta.a = 5.0f+I*5.0f;
   dta.b = 10.0+I*10.0;

   sub13(&dta);

   if ( dta.a != 10.0f+I*10.0f ) exit(313);
   if ( dta.b != 20.0+I*20.0 ) exit(317);

/* Test 14 */

   dtb.a = 5.0f+I*5.0f;
   dtb.b = 10.0+I*10.0;
   dtb.d0.a = 5.0f+I*5.0f;
   dtb.d0.b = 10.0+I*10.0;

   sub14(&dtb);

   if ( dtb.a != 10.0f+I*10.0f ) exit(321);
   if ( dtb.b != 20.0+I*20.0 ) exit(325);
   if ( dtb.d0.a != 10.0f+I*10.0f ) exit(329);
   if ( dtb.d0.b != 20.0+I*20.0 ) exit(333);

/* Test 15 */

   dtc.a = 5.0f+I*5.0f;
   dtc.b = 10.0+I*10.0;
   dtc.d1.a = 5.0f+I*5.0f;
   dtc.d1.b = 10.0+I*10.0;
   dtc.d1.d0.a = 5.0f+I*5.0f;
   dtc.d1.d0.b = 10.0+I*10.0;

   sub15(&dtc);

   if ( dtc.a != 10.0f+I*10.0f ) exit(337);
   if ( dtc.b != 20.0+I*20.0 ) exit(341);
   if ( dtc.d1.a != 10.0f+I*10.0f ) exit(345);
   if ( dtc.d1.b != 20.0+I*20.0 ) exit(349);
   if ( dtc.d1.d0.a != 10.0f+I*10.0f ) exit(353);
   if ( dtc.d1.d0.b != 20.0+I*20.0 ) exit(357);

   return 0;

}
