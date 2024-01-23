
/*
C code for testcase "fxisoo12.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

struct dt0 {
   long double _Complex a;
};

struct dt1 {
   long double _Complex a;
   struct dt0 d0;
};

struct dt2 {
   long double _Complex a;
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

   dta.a = 5.0l+I*5.0l;

   sub1(&dta);

   if ( dta.a != 10.0l+I*10.0l ) exit(21);

/* Test 2 */

   dta.a = 5.0l+I*5.0l;

   sub2(dta);

   if ( dta.a != 5.0l+I*5.0l ) exit(25);

/* Test 3 */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   sub3(&dtb);

   if ( dtb.a != 10.0l+I*10.0l ) exit(29);
   if ( dtb.d0.a != 10.0l+I*10.0l ) exit(33);

/* Test 4 */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   sub4(dtb);

   if ( dtb.a != 5.0l+I*5.0l ) exit(37);
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(41);

/* Test 5 */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   sub5(&dtc);

   if ( dtc.a != 10.0l+I*10.0l ) exit(45);
   if ( dtc.d1.a != 10.0l+I*10.0l ) exit(49);
   if ( dtc.d1.d0.a != 10.0l+I*10.0l ) exit(53);

/* Test 6 */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   sub6(dtc);

   if ( dtc.a != 5.0l+I*5.0l ) exit(57);
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(61);
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(65);

/* Test 7 */

   dta.a = 5.0l+I*5.0l;

   sub7(&dta);

   if ( dta.a != 5.0l+I*5.0l ) exit(69);

/* Test 7a */

   dta.a = 5.0l+I*5.0l;

   sub7a(&dta);

   if ( dta.a != 5.0l+I*5.0l ) exit(73);

/* Test 8 */

   dta.a = 5.0l+I*5.0l;

   sub8(dta);

   if ( dta.a != 5.0l+I*5.0l ) exit(77);

/* Test 8a */

   dta.a = 5.0l+I*5.0l;

   sub8a(dta);

   if ( dta.a != 5.0l+I*5.0l ) exit(81);

/* Test 9 */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   sub9(&dtb);

   if ( dtb.a != 5.0l+I*5.0l ) exit(85);
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(89);

/* Test 9a */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   sub9a(&dtb);

   if ( dtb.a != 5.0l+I*5.0l ) exit(93);
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(97);

/* Test 10 */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   sub10(dtb);

   if ( dtb.a != 5.0l+I*5.0l ) exit(101);
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(105);

/* Test 10a */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   sub10a(dtb);

   if ( dtb.a != 5.0l+I*5.0l ) exit(109);
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(113);

/* Test 11 */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   sub11(&dtc);

   if ( dtc.a != 5.0l+I*5.0l ) exit(117);
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(121);
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(125);

/* Test 11a */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   sub11a(&dtc);

   if ( dtc.a != 5.0l+I*5.0l ) exit(129);
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(133);
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(137);

/* Test 12 */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   sub12(dtc);

   if ( dtc.a != 5.0l+I*5.0l ) exit(143);
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(147);
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(151);

/* Test 12a */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   sub12a(dtc);

   if ( dtc.a != 5.0l+I*5.0l ) exit(155);
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(159);
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(163);

/* Test 13 */

   dta.a = 5.0l+I*5.0l;

   sub13(&dta);

   if ( dta.a != 10.0l+I*10.0l ) exit(167);

/* Test 14 */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   sub14(&dtb);

   if ( dtb.a != 10.0l+I*10.0l ) exit(171);
   if ( dtb.d0.a != 10.0l+I*10.0l ) exit(175);

/* Test 15 */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   sub15(&dtc);

   if ( dtc.a != 10.0l+I*10.0l ) exit(179);
   if ( dtc.d1.a != 10.0l+I*10.0l ) exit(183);
   if ( dtc.d1.d0.a != 10.0l+I*10.0l ) exit(187);

   return 0;

}
