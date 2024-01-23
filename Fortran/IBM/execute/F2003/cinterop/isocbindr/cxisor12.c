
/*
	C code for testcase "fxisor12.f"
*/

#include <stdio.h>
#include <stdlib.h>


struct dt0 {
   _Bool a;
};

struct dt1 {
   _Bool a;
   struct dt0 d0;
};

struct dt2 {
   _Bool a;
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

   dta.a = 1;

   ret = fnt1(&dta);

   if ( dta.a != 0 ) exit(21);

/* Test 2 */

   dta.a = 1;

   ret = fnt2(dta);

   if ( dta.a != 1 ) exit(23);

/* Test 3 */

   dtb.a = 1;
   dtb.d0.a = 1;

   ret = fnt3(&dtb);

   if ( dtb.a != 0 ) exit(25);
   if ( dtb.d0.a != 0 ) exit(27);

/* Test 4 */

   dtb.a = 1;
   dtb.d0.a = 1;

   ret = fnt4(dtb);

   if ( dtb.a != 1 ) exit(29);
   if ( dtb.d0.a != 1 ) exit(31);

/* Test 5 */

   dtc.a = 1;
   dtc.d1.a = 1;
   dtc.d1.d0.a = 1;

   ret = fnt5(&dtc);

   if ( dtc.a != 0 ) exit(33);
   if ( dtc.d1.a != 0 ) exit(35);
   if ( dtc.d1.d0.a != 0 ) exit(37);

/* Test 6 */

   dtc.a = 1;
   dtc.d1.a = 1;
   dtc.d1.d0.a = 1;

   ret = fnt6(dtc);

   if ( dtc.a != 1 ) exit(39);
   if ( dtc.d1.a != 1 ) exit(41);
   if ( dtc.d1.d0.a != 1 ) exit(43);

/* Test 7 */

   dta.a = 1;

   ret = fnt7(&dta);

   if ( dta.a != 1 ) exit(45);

/* Test 7a */

   dta.a = 1;

   ret = fnt7a(&dta);

   if ( dta.a != 1 ) exit(47);

/* Test 8 */

   dta.a = 1;

   ret = fnt8(dta);

   if ( dta.a != 1 ) exit(49);

/* Test 8a */

   dta.a = 1;

   ret = fnt8a(dta);

   if ( dta.a != 1 ) exit(51);

/* Test 9 */

   dtb.a = 1;
   dtb.d0.a = 1;

   ret = fnt9(&dtb);

   if ( dtb.a != 1 ) exit(53);
   if ( dtb.d0.a != 1 ) exit(55);

/* Test 9a */

   dtb.a = 1;
   dtb.d0.a = 1;

   ret = fnt9a(&dtb);

   if ( dtb.a != 1 ) exit(57);
   if ( dtb.d0.a != 1 ) exit(59);

/* Test 10 */

   dtb.a = 1;
   dtb.d0.a = 1;

   ret = fnt10(dtb);

   if ( dtb.a != 1 ) exit(61);
   if ( dtb.d0.a != 1 ) exit(63);

/* Test 10a */

   dtb.a = 1;
   dtb.d0.a = 1;

   ret = fnt10a(dtb);

   if ( dtb.a != 1 ) exit(65);
   if ( dtb.d0.a != 1 ) exit(67);

/* Test 11 */

   dtc.a = 1;
   dtc.d1.a = 1;
   dtc.d1.d0.a = 1;

   ret = fnt11(&dtc);

   if ( dtc.a != 1 ) exit(69);
   if ( dtc.d1.a != 1 ) exit(71);
   if ( dtc.d1.d0.a != 1 ) exit(73);

/* Test 11a */

   dtc.a = 1;
   dtc.d1.a = 1;
   dtc.d1.d0.a = 1;

   ret = fnt11a(&dtc);

   if ( dtc.a != 1 ) exit(75);
   if ( dtc.d1.a != 1 ) exit(77);
   if ( dtc.d1.d0.a != 1 ) exit(79);

/* Test 12 */

   dtc.a = 1;
   dtc.d1.a = 1;
   dtc.d1.d0.a = 1;

   ret = fnt12(dtc);

   if ( dtc.a != 1 ) exit(81);
   if ( dtc.d1.a != 1 ) exit(83);
   if ( dtc.d1.d0.a != 1 ) exit(85);

/* Test 12a */

   dtc.a = 1;
   dtc.d1.a = 1;
   dtc.d1.d0.a = 1;

   ret = fnt12a(dtc);

   if ( dtc.a != 1 ) exit(87);
   if ( dtc.d1.a != 1 ) exit(89);
   if ( dtc.d1.d0.a != 1 ) exit(91);

/* Test 13 */

   dta.a = 1;

   ret = fnt13(&dta);

   if ( dta.a != 0 ) exit(93);

/* Test 14 */

   dtb.a = 1;
   dtb.d0.a = 1;

   ret = fnt14(&dtb);

   if ( dtb.a != 0 ) exit(95);
   if ( dtb.d0.a != 0 ) exit(97);

/* Test 15 */

   dtc.a = 1;
   dtc.d1.a = 1;
   dtc.d1.d0.a = 1;

   ret = fnt15(&dtc);

   if ( dtc.a != 0 ) exit(99);
   if ( dtc.d1.a != 0 ) exit(101);
   if ( dtc.d1.d0.a != 0 ) exit(103);

   return 0;

}
