
/*
	C code for testcase "fxisog12.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

struct dt0 {
   int_fast8_t a;
   intmax_t b;
};

struct dt1 {
   int_fast8_t a;
   intmax_t b;
   struct dt0 d0;
};

struct dt2 {
   int_fast8_t a;
   intmax_t b;
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

   dta.a = 5;
   dta.b = 10;

   sub1(&dta);

   if ( dta.a != 10 ) exit(21);
   if ( dta.b != 20 ) exit(23);

/* Test 2 */

   dta.a = 5;
   dta.b = 10;

   sub2(dta);

   if ( dta.a != 5 ) exit(25);
   if ( dta.b != 10 ) exit(27);

/* Test 3 */

   dtb.a = 5;
   dtb.b = 10;
   dtb.d0.a = 5;
   dtb.d0.b = 10;

   sub3(&dtb);

   if ( dtb.a != 10 ) exit(29);
   if ( dtb.b != 20 ) exit(31);
   if ( dtb.d0.a != 10 ) exit(33);
   if ( dtb.d0.b != 20 ) exit(35);

/* Test 4 */

   dtb.a = 5;
   dtb.b = 10;
   dtb.d0.a = 5;
   dtb.d0.b = 10;

   sub4(dtb);

   if ( dtb.a != 5 ) exit(37);
   if ( dtb.b != 10 ) exit(39);
   if ( dtb.d0.a != 5 ) exit(41);
   if ( dtb.d0.b != 10 ) exit(43);

/* Test 5 */

   dtc.a = 5;
   dtc.b = 10;
   dtc.d1.a = 5;
   dtc.d1.b = 10;
   dtc.d1.d0.a = 5;
   dtc.d1.d0.b = 10;

   sub5(&dtc);

   if ( dtc.a != 10 ) exit(45);
   if ( dtc.b != 20 ) exit(47);
   if ( dtc.d1.a != 10 ) exit(49);
   if ( dtc.d1.b != 20 ) exit(51);
   if ( dtc.d1.d0.a != 10 ) exit(53);
   if ( dtc.d1.d0.b != 20 ) exit(55);

/* Test 6 */

   dtc.a = 5;
   dtc.b = 10;
   dtc.d1.a = 5;
   dtc.d1.b = 10;
   dtc.d1.d0.a = 5;
   dtc.d1.d0.b = 10;

   sub6(dtc);

   if ( dtc.a != 5 ) exit(57);
   if ( dtc.b != 10 ) exit(59);
   if ( dtc.d1.a != 5 ) exit(61);
   if ( dtc.d1.b != 10 ) exit(63);
   if ( dtc.d1.d0.a != 5 ) exit(65);
   if ( dtc.d1.d0.b != 10 ) exit(67);

/* Test 7 */

   dta.a = 5;
   dta.b = 10;

   sub7(&dta);

   if ( dta.a != 5 ) exit(69);
   if ( dta.b != 10 ) exit(71);

/* Test 7a */

   dta.a = 5;
   dta.b = 10;

   sub7a(&dta);

   if ( dta.a != 5 ) exit(73);
   if ( dta.b != 10 ) exit(75);

/* Test 8 */

   dta.a = 5;
   dta.b = 10;

   sub8(dta);

   if ( dta.a != 5 ) exit(77);
   if ( dta.b != 10 ) exit(79);

/* Test 8a */

   dta.a = 5;
   dta.b = 10;

   sub8a(dta);

   if ( dta.a != 5 ) exit(81);
   if ( dta.b != 10 ) exit(83);

/* Test 9 */

   dtb.a = 5;
   dtb.b = 10;
   dtb.d0.a = 5;
   dtb.d0.b = 10;

   sub9(&dtb);

   if ( dtb.a != 5 ) exit(85);
   if ( dtb.b != 10 ) exit(87);
   if ( dtb.d0.a != 5 ) exit(89);
   if ( dtb.d0.b != 10 ) exit(91);

/* Test 9a */

   dtb.a = 5;
   dtb.b = 10;
   dtb.d0.a = 5;
   dtb.d0.b = 10;

   sub9a(&dtb);

   if ( dtb.a != 5 ) exit(93);
   if ( dtb.b != 10 ) exit(95);
   if ( dtb.d0.a != 5 ) exit(97);
   if ( dtb.d0.b != 10 ) exit(99);

/* Test 10 */

   dtb.a = 5;
   dtb.b = 10;
   dtb.d0.a = 5;
   dtb.d0.b = 10;

   sub10(dtb);

   if ( dtb.a != 5 ) exit(101);
   if ( dtb.b != 10 ) exit(103);
   if ( dtb.d0.a != 5 ) exit(105);
   if ( dtb.d0.b != 10 ) exit(107);

/* Test 10a */

   dtb.a = 5;
   dtb.b = 10;
   dtb.d0.a = 5;
   dtb.d0.b = 10;

   sub10a(dtb);

   if ( dtb.a != 5 ) exit(109);
   if ( dtb.b != 10 ) exit(111);
   if ( dtb.d0.a != 5 ) exit(113);
   if ( dtb.d0.b != 10 ) exit(115);

/* Test 11 */

   dtc.a = 5;
   dtc.b = 10;
   dtc.d1.a = 5;
   dtc.d1.b = 10;
   dtc.d1.d0.a = 5;
   dtc.d1.d0.b = 10;

   sub11(&dtc);

   if ( dtc.a != 5 ) exit(117);
   if ( dtc.b != 10 ) exit(119);
   if ( dtc.d1.a != 5 ) exit(121);
   if ( dtc.d1.b != 10 ) exit(123);
   if ( dtc.d1.d0.a != 5 ) exit(125);
   if ( dtc.d1.d0.b != 10 ) exit(127);

/* Test 11a */

   dtc.a = 5;
   dtc.b = 10;
   dtc.d1.a = 5;
   dtc.d1.b = 10;
   dtc.d1.d0.a = 5;
   dtc.d1.d0.b = 10;

   sub11a(&dtc);

   if ( dtc.a != 5 ) exit(129);
   if ( dtc.b != 10 ) exit(131);
   if ( dtc.d1.a != 5 ) exit(133);
   if ( dtc.d1.b != 10 ) exit(135);
   if ( dtc.d1.d0.a != 5 ) exit(137);
   if ( dtc.d1.d0.b != 10 ) exit(141);

/* Test 12 */

   dtc.a = 5;
   dtc.b = 10;
   dtc.d1.a = 5;
   dtc.d1.b = 10;
   dtc.d1.d0.a = 5;
   dtc.d1.d0.b = 10;

   sub12(dtc);

   if ( dtc.a != 5 ) exit(143);
   if ( dtc.b != 10 ) exit(145);
   if ( dtc.d1.a != 5 ) exit(147);
   if ( dtc.d1.b != 10 ) exit(149);
   if ( dtc.d1.d0.a != 5 ) exit(151);
   if ( dtc.d1.d0.b != 10 ) exit(153);

/* Test 12a */

   dtc.a = 5;
   dtc.b = 10;
   dtc.d1.a = 5;
   dtc.d1.b = 10;
   dtc.d1.d0.a = 5;
   dtc.d1.d0.b = 10;

   sub12a(dtc);

   if ( dtc.a != 5 ) exit(155);
   if ( dtc.b != 10 ) exit(157);
   if ( dtc.d1.a != 5 ) exit(159);
   if ( dtc.d1.b != 10 ) exit(161);
   if ( dtc.d1.d0.a != 5 ) exit(163);
   if ( dtc.d1.d0.b != 10 ) exit(165);

/* Test 13 */

   dta.a = 5;
   dta.b = 10;

   sub13(&dta);

   if ( dta.a != 10 ) exit(167);
   if ( dta.b != 20 ) exit(169);

/* Test 14 */

   dtb.a = 5;
   dtb.b = 10;
   dtb.d0.a = 5;
   dtb.d0.b = 10;

   sub14(&dtb);

   if ( dtb.a != 10 ) exit(171);
   if ( dtb.b != 20 ) exit(173);
   if ( dtb.d0.a != 10 ) exit(175);
   if ( dtb.d0.b != 20 ) exit(177);

/* Test 15 */

   dtc.a = 5;
   dtc.b = 10;
   dtc.d1.a = 5;
   dtc.d1.b = 10;
   dtc.d1.d0.a = 5;
   dtc.d1.d0.b = 10;

   sub15(&dtc);

   if ( dtc.a != 10 ) exit(179);
   if ( dtc.b != 20 ) exit(181);
   if ( dtc.d1.a != 10 ) exit(183);
   if ( dtc.d1.b != 20 ) exit(185);
   if ( dtc.d1.d0.a != 10 ) exit(187);
   if ( dtc.d1.d0.b != 20 ) exit(189);

   return 0;

}
