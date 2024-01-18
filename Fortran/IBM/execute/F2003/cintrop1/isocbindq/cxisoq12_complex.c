
/*
C code for testcase "fxisoq12.f"
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

   int fnt1(struct dt0 *);
   int fnt2(struct dt0);
   int fnt3(struct dt1 *);
   int fnt4(struct dt1);
   int fnt5(struct dt2 *);
   int fnt6(struct dt2);
   int fnt7(struct dt0 *);
   int fnt7a(const struct dt0 *);
   int fnt8(struct dt0);
   int fnt8a(const struct dt0);
   int fnt9(struct dt1 *);
   int fnt9a(const struct dt1 *);
   int fnt10(struct dt1);
   int fnt10a(const struct dt1);
   int fnt11(struct dt2 *);
   int fnt11a(const struct dt2 *);
   int fnt12(struct dt2);
   int fnt12a(const struct dt2);
   int fnt13(struct dt0 *);
   int fnt14(struct dt1 *);
   int fnt15(struct dt2 *);

   struct dt0 dta;
   struct dt1 dtb;
   struct dt2 dtc;
   int ret;

/* Test 1 */

   dta.a = 5.0l+I*5.0l;

   ret = fnt1(&dta);

   if ( dta.a != 10.0l+I*10.0l ) exit(21);

/* Test 2 */

   dta.a = 5.0l+I*5.0l;

   ret = fnt2(dta);

   if ( dta.a != 5.0l+I*5.0l ) exit(25);

/* Test 3 */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   ret = fnt3(&dtb);

   if ( dtb.a != 10.0l+I*10.0l ) exit(29);
   if ( dtb.d0.a != 10.0l+I*10.0l ) exit(33);

/* Test 4 */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   ret = fnt4(dtb);

   if ( dtb.a != 5.0l+I*5.0l ) exit(37);
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(41);

/* Test 5 */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   ret = fnt5(&dtc);

   if ( dtc.a != 10.0l+I*10.0l ) exit(45);
   if ( dtc.d1.a != 10.0l+I*10.0l ) exit(49);
   if ( dtc.d1.d0.a != 10.0l+I*10.0l ) exit(53);

/* Test 6 */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   ret = fnt6(dtc);

   if ( dtc.a != 5.0l+I*5.0l ) exit(57);
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(61);
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(65);

/* Test 7 */

   dta.a = 5.0l+I*5.0l;

   ret = fnt7(&dta);

   if ( dta.a != 5.0l+I*5.0l ) exit(69);

/* Test 7a */

   dta.a = 5.0l+I*5.0l;

   ret = fnt7a(&dta);

   if ( dta.a != 5.0l+I*5.0l ) exit(73);

/* Test 8 */

   dta.a = 5.0l+I*5.0l;

   ret = fnt8(dta);

   if ( dta.a != 5.0l+I*5.0l ) exit(77);

/* Test 8a */

   dta.a = 5.0l+I*5.0l;

   ret = fnt8a(dta);

   if ( dta.a != 5.0l+I*5.0l ) exit(81);

/* Test 9 */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   ret = fnt9(&dtb);

   if ( dtb.a != 5.0l+I*5.0l ) exit(85);
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(89);

/* Test 9a */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   ret = fnt9a(&dtb);

   if ( dtb.a != 5.0l+I*5.0l ) exit(93);
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(97);

/* Test 10 */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   ret = fnt10(dtb);

   if ( dtb.a != 5.0l+I*5.0l ) exit(101);
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(105);

/* Test 10a */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   ret = fnt10a(dtb);

   if ( dtb.a != 5.0l+I*5.0l ) exit(109);
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(113);

/* Test 11 */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   ret = fnt11(&dtc);

   if ( dtc.a != 5.0l+I*5.0l ) exit(117);
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(121);
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(125);

/* Test 11a */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   ret = fnt11a(&dtc);

   if ( dtc.a != 5.0l+I*5.0l ) exit(129);
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(133);
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(137);

/* Test 12 */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   ret = fnt12(dtc);

   if ( dtc.a != 5.0l+I*5.0l ) exit(143);
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(147);
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(151);

/* Test 12a */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   ret = fnt12a(dtc);

   if ( dtc.a != 5.0l+I*5.0l ) exit(155);
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(159);
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(163);

/* Test 13 */

   dta.a = 5.0l+I*5.0l;

   ret = fnt13(&dta);

   if ( dta.a != 10.0l+I*10.0l ) exit(167);

/* Test 14 */

   dtb.a = 5.0l+I*5.0l;
   dtb.d0.a = 5.0l+I*5.0l;

   ret = fnt14(&dtb);

   if ( dtb.a != 10.0l+I*10.0l ) exit(171);
   if ( dtb.d0.a != 10.0l+I*10.0l ) exit(175);

/* Test 15 */

   dtc.a = 5.0l+I*5.0l;
   dtc.d1.a = 5.0l+I*5.0l;
   dtc.d1.d0.a = 5.0l+I*5.0l;

   ret = fnt15(&dtc);

   if ( dtc.a != 10.0l+I*10.0l ) exit(179);
   if ( dtc.d1.a != 10.0l+I*10.0l ) exit(183);
   if ( dtc.d1.d0.a != 10.0l+I*10.0l ) exit(187);

   return 0;

}
