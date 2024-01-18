
/*
	C code for testcase "fxisok12.f"
*/

#include <stdio.h>
#include <stdlib.h>

struct dt0 {
   char a;
   signed char b;
};

struct dt1 {
   char a;
   signed char b;
   struct dt0 d0;
};

struct dt2 {
   char a;
   signed char b;
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

   dta.a = 'A';
   dta.b = 'B';

   ret = fnt1(&dta);

   if ( dta.a != 'C' ) exit(21);
   if ( dta.b != 'D' ) exit(23);

/* Test 2 */

   dta.a = 'A';
   dta.b = 'B';

   ret = fnt2(dta);

   if ( dta.a != 'A' ) exit(25);
   if ( dta.b != 'B' ) exit(27);

/* Test 3 */

   dtb.a = 'A';
   dtb.b = 'B';
   dtb.d0.a = 'A';
   dtb.d0.b = 'B';

   ret = fnt3(&dtb);

   if ( dtb.a != 'C' ) exit(29);
   if ( dtb.b != 'D' ) exit(31);
   if ( dtb.d0.a != 'C' ) exit(33);
   if ( dtb.d0.b != 'D' ) exit(35);

/* Test 4 */

   dtb.a = 'A';
   dtb.b = 'B';
   dtb.d0.a = 'A';
   dtb.d0.b = 'B';

   ret = fnt4(dtb);

   if ( dtb.a != 'A' ) exit(37);
   if ( dtb.b != 'B' ) exit(39);
   if ( dtb.d0.a != 'A' ) exit(41);
   if ( dtb.d0.b != 'B' ) exit(43);

/* Test 5 */

   dtc.a = 'A';
   dtc.b = 'B';
   dtc.d1.a = 'A';
   dtc.d1.b = 'B';
   dtc.d1.d0.a = 'A';
   dtc.d1.d0.b = 'B';

   ret = fnt5(&dtc);

   if ( dtc.a != 'C' ) exit(45);
   if ( dtc.b != 'D' ) exit(47);
   if ( dtc.d1.a != 'C' ) exit(49);
   if ( dtc.d1.b != 'D' ) exit(51);
   if ( dtc.d1.d0.a != 'C' ) exit(53);
   if ( dtc.d1.d0.b != 'D' ) exit(55);

/* Test 6 */

   dtc.a = 'A';
   dtc.b = 'B';
   dtc.d1.a = 'A';
   dtc.d1.b = 'B';
   dtc.d1.d0.a = 'A';
   dtc.d1.d0.b = 'B';

   ret = fnt6(dtc);

   if ( dtc.a != 'A' ) exit(57);
   if ( dtc.b != 'B' ) exit(59);
   if ( dtc.d1.a != 'A' ) exit(61);
   if ( dtc.d1.b != 'B' ) exit(63);
   if ( dtc.d1.d0.a != 'A' ) exit(65);
   if ( dtc.d1.d0.b != 'B' ) exit(67);

/* Test 7 */

   dta.a = 'A';
   dta.b = 'B';

   ret = fnt7(&dta);

   if ( dta.a != 'A' ) exit(69);
   if ( dta.b != 'B' ) exit(71);

/* Test 7a */

   dta.a = 'A';
   dta.b = 'B';

   ret = fnt7a(&dta);

   if ( dta.a != 'A' ) exit(73);
   if ( dta.b != 'B' ) exit(75);

/* Test 8 */

   dta.a = 'A';
   dta.b = 'B';

   ret = fnt8(dta);

   if ( dta.a != 'A' ) exit(77);
   if ( dta.b != 'B' ) exit(79);

/* Test 8a */

   dta.a = 'A';
   dta.b = 'B';

   ret = fnt8a(dta);

   if ( dta.a != 'A' ) exit(81);
   if ( dta.b != 'B' ) exit(83);

/* Test 9 */

   dtb.a = 'A';
   dtb.b = 'B';
   dtb.d0.a = 'A';
   dtb.d0.b = 'B';

   ret = fnt9(&dtb);

   if ( dtb.a != 'A' ) exit(85);
   if ( dtb.b != 'B' ) exit(87);
   if ( dtb.d0.a != 'A' ) exit(89);
   if ( dtb.d0.b != 'B' ) exit(91);

/* Test 9a */

   dtb.a = 'A';
   dtb.b = 'B';
   dtb.d0.a = 'A';
   dtb.d0.b = 'B';

   ret = fnt9a(&dtb);

   if ( dtb.a != 'A' ) exit(93);
   if ( dtb.b != 'B' ) exit(95);
   if ( dtb.d0.a != 'A' ) exit(97);
   if ( dtb.d0.b != 'B' ) exit(99);

/* Test 10 */

   dtb.a = 'A';
   dtb.b = 'B';
   dtb.d0.a = 'A';
   dtb.d0.b = 'B';

   ret = fnt10(dtb);

   if ( dtb.a != 'A' ) exit(101);
   if ( dtb.b != 'B' ) exit(103);
   if ( dtb.d0.a != 'A' ) exit(105);
   if ( dtb.d0.b != 'B' ) exit(107);

/* Test 10a */

   dtb.a = 'A';
   dtb.b = 'B';
   dtb.d0.a = 'A';
   dtb.d0.b = 'B';

   ret = fnt10a(dtb);

   if ( dtb.a != 'A' ) exit(109);
   if ( dtb.b != 'B' ) exit(111);
   if ( dtb.d0.a != 'A' ) exit(113);
   if ( dtb.d0.b != 'B' ) exit(115);

/* Test 11 */

   dtc.a = 'A';
   dtc.b = 'B';
   dtc.d1.a = 'A';
   dtc.d1.b = 'B';
   dtc.d1.d0.a = 'A';
   dtc.d1.d0.b = 'B';

   ret = fnt11(&dtc);

   if ( dtc.a != 'A' ) exit(117);
   if ( dtc.b != 'B' ) exit(119);
   if ( dtc.d1.a != 'A' ) exit(121);
   if ( dtc.d1.b != 'B' ) exit(123);
   if ( dtc.d1.d0.a != 'A' ) exit(125);
   if ( dtc.d1.d0.b != 'B' ) exit(127);

/* Test 11a */

   dtc.a = 'A';
   dtc.b = 'B';
   dtc.d1.a = 'A';
   dtc.d1.b = 'B';
   dtc.d1.d0.a = 'A';
   dtc.d1.d0.b = 'B';

   ret = fnt11a(&dtc);

   if ( dtc.a != 'A' ) exit(129);
   if ( dtc.b != 'B' ) exit(131);
   if ( dtc.d1.a != 'A' ) exit(133);
   if ( dtc.d1.b != 'B' ) exit(135);
   if ( dtc.d1.d0.a != 'A' ) exit(137);
   if ( dtc.d1.d0.b != 'B' ) exit(141);

/* Test 12 */

   dtc.a = 'A';
   dtc.b = 'B';
   dtc.d1.a = 'A';
   dtc.d1.b = 'B';
   dtc.d1.d0.a = 'A';
   dtc.d1.d0.b = 'B';

   ret = fnt12(dtc);

   if ( dtc.a != 'A' ) exit(143);
   if ( dtc.b != 'B' ) exit(145);
   if ( dtc.d1.a != 'A' ) exit(147);
   if ( dtc.d1.b != 'B' ) exit(149);
   if ( dtc.d1.d0.a != 'A' ) exit(151);
   if ( dtc.d1.d0.b != 'B' ) exit(153);

/* Test 12a */

   dtc.a = 'A';
   dtc.b = 'B';
   dtc.d1.a = 'A';
   dtc.d1.b = 'B';
   dtc.d1.d0.a = 'A';
   dtc.d1.d0.b = 'B';

   ret = fnt12a(dtc);

   if ( dtc.a != 'A' ) exit(155);
   if ( dtc.b != 'B' ) exit(157);
   if ( dtc.d1.a != 'A' ) exit(159);
   if ( dtc.d1.b != 'B' ) exit(161);
   if ( dtc.d1.d0.a != 'A' ) exit(163);
   if ( dtc.d1.d0.b != 'B' ) exit(165);

/* Test 13 */

   dta.a = 'A';
   dta.b = 'B';

   ret = fnt13(&dta);

   if ( dta.a != 'C' ) exit(167);
   if ( dta.b != 'D' ) exit(169);

/* Test 14 */

   dtb.a = 'A';
   dtb.b = 'B';
   dtb.d0.a = 'A';
   dtb.d0.b = 'B';

   ret = fnt14(&dtb);

   if ( dtb.a != 'C' ) exit(171);
   if ( dtb.b != 'D' ) exit(173);
   if ( dtb.d0.a != 'C' ) exit(175);
   if ( dtb.d0.b != 'D' ) exit(177);

/* Test 15 */

   dtc.a = 'A';
   dtc.b = 'B';
   dtc.d1.a = 'A';
   dtc.d1.b = 'B';
   dtc.d1.d0.a = 'A';
   dtc.d1.d0.b = 'B';

   ret = fnt15(&dtc);

   if ( dtc.a != 'C' ) exit(179);
   if ( dtc.b != 'D' ) exit(181);
   if ( dtc.d1.a != 'C' ) exit(183);
   if ( dtc.d1.b != 'D' ) exit(185);
   if ( dtc.d1.d0.a != 'C' ) exit(187);
   if ( dtc.d1.d0.b != 'D' ) exit(189);

   return 0;

}
