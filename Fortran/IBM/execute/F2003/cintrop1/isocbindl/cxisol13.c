
/*
	C code for testcase "fxisol12.f"
*/

#include <stdio.h>
#include <stdlib.h>


struct dt0 {
   float a;
   double b;
};

struct dt1 {
   float a;
   double b;
   struct dt0 d0;
};

struct dt2 {
   float a;
   double b;
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

   dta.a = 5.0f;
   dta.b = 10.0;

   sub1(&dta);

   if ( dta.a != 10.0f ) exit(21);
   if ( dta.b != 20.0 ) exit(23);

/* Test 2 */

   dta.a = 5.0f;
   dta.b = 10.0;

   sub2(dta);

   if ( dta.a != 5.0f ) exit(25);
   if ( dta.b != 10.0 ) exit(27);

/* Test 3 */

   dtb.a = 5.0f;
   dtb.b = 10.0;
   dtb.d0.a = 5.0f;
   dtb.d0.b = 10.0;

   sub3(&dtb);

   if ( dtb.a != 10.0f ) exit(29);
   if ( dtb.b != 20.0 ) exit(31);
   if ( dtb.d0.a != 10.0f ) exit(33);
   if ( dtb.d0.b != 20.0 ) exit(35);

/* Test 4 */

   dtb.a = 5.0f;
   dtb.b = 10.0;
   dtb.d0.a = 5.0f;
   dtb.d0.b = 10.0;

   sub4(dtb);

   if ( dtb.a != 5.0f ) exit(37);
   if ( dtb.b != 10.0 ) exit(39);
   if ( dtb.d0.a != 5.0f ) exit(41);
   if ( dtb.d0.b != 10.0 ) exit(43);

/* Test 5 */

   dtc.a = 5.0f;
   dtc.b = 10.0;
   dtc.d1.a = 5.0f;
   dtc.d1.b = 10.0;
   dtc.d1.d0.a = 5.0f;
   dtc.d1.d0.b = 10.0;

   sub5(&dtc);

   if ( dtc.a != 10.0f ) exit(45);
   if ( dtc.b != 20.0 ) exit(47);
   if ( dtc.d1.a != 10.0f ) exit(49);
   if ( dtc.d1.b != 20.0 ) exit(51);
   if ( dtc.d1.d0.a != 10.0f ) exit(53);
   if ( dtc.d1.d0.b != 20.0 ) exit(55);

/* Test 6 */

   dtc.a = 5.0f;
   dtc.b = 10.0;
   dtc.d1.a = 5.0f;
   dtc.d1.b = 10.0;
   dtc.d1.d0.a = 5.0f;
   dtc.d1.d0.b = 10.0;

   sub6(dtc);

   if ( dtc.a != 5.0f ) exit(57);
   if ( dtc.b != 10.0 ) exit(59);
   if ( dtc.d1.a != 5.0f ) exit(61);
   if ( dtc.d1.b != 10.0 ) exit(63);
   if ( dtc.d1.d0.a != 5.0f ) exit(65);
   if ( dtc.d1.d0.b != 10.0 ) exit(67);

/* Test 7 */

   dta.a = 5.0f;
   dta.b = 10.0;

   sub7(&dta);

   if ( dta.a != 5.0f ) exit(69);
   if ( dta.b != 10.0 ) exit(71);

/* Test 7a */

   dta.a = 5.0f;
   dta.b = 10.0;

   sub7a(&dta);

   if ( dta.a != 5.0f ) exit(73);
   if ( dta.b != 10.0 ) exit(75);

/* Test 8 */

   dta.a = 5.0f;
   dta.b = 10.0;

   sub8(dta);

   if ( dta.a != 5.0f ) exit(77);
   if ( dta.b != 10.0 ) exit(79);

/* Test 8a */

   dta.a = 5.0f;
   dta.b = 10.0;

   sub8a(dta);

   if ( dta.a != 5.0f ) exit(81);
   if ( dta.b != 10.0 ) exit(83);

/* Test 9 */

   dtb.a = 5.0f;
   dtb.b = 10.0;
   dtb.d0.a = 5.0f;
   dtb.d0.b = 10.0;

   sub9(&dtb);

   if ( dtb.a != 5.0f ) exit(85);
   if ( dtb.b != 10.0 ) exit(87);
   if ( dtb.d0.a != 5.0f ) exit(89);
   if ( dtb.d0.b != 10.0 ) exit(91);

/* Test 9a */

   dtb.a = 5.0f;
   dtb.b = 10.0;
   dtb.d0.a = 5.0f;
   dtb.d0.b = 10.0;

   sub9a(&dtb);

   if ( dtb.a != 5.0f ) exit(93);
   if ( dtb.b != 10.0 ) exit(95);
   if ( dtb.d0.a != 5.0f ) exit(97);
   if ( dtb.d0.b != 10.0 ) exit(99);

/* Test 10 */

   dtb.a = 5.0f;
   dtb.b = 10.0;
   dtb.d0.a = 5.0f;
   dtb.d0.b = 10.0;

   sub10(dtb);

   if ( dtb.a != 5.0f ) exit(101);
   if ( dtb.b != 10.0 ) exit(103);
   if ( dtb.d0.a != 5.0f ) exit(105);
   if ( dtb.d0.b != 10.0 ) exit(107);

/* Test 10a */

   dtb.a = 5.0f;
   dtb.b = 10.0;
   dtb.d0.a = 5.0f;
   dtb.d0.b = 10.0;

   sub10a(dtb);

   if ( dtb.a != 5.0f ) exit(109);
   if ( dtb.b != 10.0 ) exit(111);
   if ( dtb.d0.a != 5.0f ) exit(113);
   if ( dtb.d0.b != 10.0 ) exit(115);

/* Test 11 */

   dtc.a = 5.0f;
   dtc.b = 10.0;
   dtc.d1.a = 5.0f;
   dtc.d1.b = 10.0;
   dtc.d1.d0.a = 5.0f;
   dtc.d1.d0.b = 10.0;

   sub11(&dtc);

   if ( dtc.a != 5.0f ) exit(117);
   if ( dtc.b != 10.0 ) exit(119);
   if ( dtc.d1.a != 5.0f ) exit(121);
   if ( dtc.d1.b != 10.0 ) exit(123);
   if ( dtc.d1.d0.a != 5.0f ) exit(125);
   if ( dtc.d1.d0.b != 10.0 ) exit(127);

/* Test 11a */

   dtc.a = 5.0f;
   dtc.b = 10.0;
   dtc.d1.a = 5.0f;
   dtc.d1.b = 10.0;
   dtc.d1.d0.a = 5.0f;
   dtc.d1.d0.b = 10.0;

   sub11a(&dtc);

   if ( dtc.a != 5.0f ) exit(129);
   if ( dtc.b != 10.0 ) exit(131);
   if ( dtc.d1.a != 5.0f ) exit(133);
   if ( dtc.d1.b != 10.0 ) exit(135);
   if ( dtc.d1.d0.a != 5.0f ) exit(137);
   if ( dtc.d1.d0.b != 10.0 ) exit(141);

/* Test 12 */

   dtc.a = 5.0f;
   dtc.b = 10.0;
   dtc.d1.a = 5.0f;
   dtc.d1.b = 10.0;
   dtc.d1.d0.a = 5.0f;
   dtc.d1.d0.b = 10.0;

   sub12(dtc);

   if ( dtc.a != 5.0f ) exit(143);
   if ( dtc.b != 10.0 ) exit(145);
   if ( dtc.d1.a != 5.0f ) exit(147);
   if ( dtc.d1.b != 10.0 ) exit(149);
   if ( dtc.d1.d0.a != 5.0f ) exit(151);
   if ( dtc.d1.d0.b != 10.0 ) exit(153);

/* Test 12a */

   dtc.a = 5.0f;
   dtc.b = 10.0;
   dtc.d1.a = 5.0f;
   dtc.d1.b = 10.0;
   dtc.d1.d0.a = 5.0f;
   dtc.d1.d0.b = 10.0;

   sub12a(dtc);

   if ( dtc.a != 5.0f ) exit(155);
   if ( dtc.b != 10.0 ) exit(157);
   if ( dtc.d1.a != 5.0f ) exit(159);
   if ( dtc.d1.b != 10.0 ) exit(161);
   if ( dtc.d1.d0.a != 5.0f ) exit(163);
   if ( dtc.d1.d0.b != 10.0 ) exit(165);

/* Test 13 */

   dta.a = 5.0f;
   dta.b = 10.0;

   sub13(&dta);

   if ( dta.a != 10.0f ) exit(167);
   if ( dta.b != 20.0 ) exit(169);

/* Test 14 */

   dtb.a = 5.0f;
   dtb.b = 10.0;
   dtb.d0.a = 5.0f;
   dtb.d0.b = 10.0;

   sub14(&dtb);

   if ( dtb.a != 10.0f ) exit(171);
   if ( dtb.b != 20.0 ) exit(173);
   if ( dtb.d0.a != 10.0f ) exit(175);
   if ( dtb.d0.b != 20.0 ) exit(177);

/* Test 15 */

   dtc.a = 5.0f;
   dtc.b = 10.0;
   dtc.d1.a = 5.0f;
   dtc.d1.b = 10.0;
   dtc.d1.d0.a = 5.0f;
   dtc.d1.d0.b = 10.0;

   sub15(&dtc);

   if ( dtc.a != 10.0f ) exit(179);
   if ( dtc.b != 20.0 ) exit(181);
   if ( dtc.d1.a != 10.0f ) exit(183);
   if ( dtc.d1.b != 20.0 ) exit(185);
   if ( dtc.d1.d0.a != 10.0f ) exit(187);
   if ( dtc.d1.d0.b != 20.0 ) exit(189);

   return 0;

}
