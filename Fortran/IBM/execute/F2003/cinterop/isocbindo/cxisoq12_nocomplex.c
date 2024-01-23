
/*
C code for testcase "fxisoq12.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

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

   dta.a = createcomplexl(5.0l,5.0l);

   ret = fnt1(&dta);

   if ( dta.a != createcomplexl(10.0l,10.0l) ) exit(23);

/* Test 2 */

   dta.a = createcomplexl(5.0l,5.0l);

   ret = fnt2(dta);

   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(27);

/* Test 3 */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt3(&dtb);

   if ( dtb.a != createcomplexl(10.0l,10.0l) ) exit(31);
   if ( dtb.d0.a != createcomplexl(10.0l,10.0l) ) exit(35);

/* Test 4 */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt4(dtb);

   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(39);
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(43);

/* Test 5 */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt5(&dtc);

   if ( dtc.a != createcomplexl(10.0l,10.0l) ) exit(47);
   if ( dtc.d1.a != createcomplexl(10.0l,10.0l) ) exit(51);
   if ( dtc.d1.d0.a != createcomplexl(10.0l,10.0l) ) exit(55);

/* Test 6 */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt6(dtc);

   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(59);
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(63);
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(67);

/* Test 7 */

   dta.a = createcomplexl(5.0l,5.0l);

   ret = fnt7(&dta);

   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(71);

/* Test 7a */

   dta.a = createcomplexl(5.0l,5.0l);

   ret = fnt7a(&dta);

   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(75);

/* Test 8 */

   dta.a = createcomplexl(5.0l,5.0l);

   ret = fnt8(dta);

   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(79);

/* Test 8a */

   dta.a = createcomplexl(5.0l,5.0l);

   ret = fnt8a(dta);

   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(83);

/* Test 9 */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt9(&dtb);

   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(87);
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(91);

/* Test 9a */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt9a(&dtb);

   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(95);
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(99);

/* Test 10 */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt10(dtb);

   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(103);
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(107);

/* Test 10a */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt10a(dtb);

   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(111);
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(115);

/* Test 11 */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt11(&dtc);

   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(119);
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(123);
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(127);

/* Test 11a */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt11a(&dtc);

   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(131);
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(135);
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(141);

/* Test 12 */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt12(dtc);

   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(145);
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(149);
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(153);

/* Test 12a */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt12a(dtc);

   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(157);
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(161);
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(165);

/* Test 13 */

   dta.a = createcomplexl(5.0l,5.0l);

   ret = fnt13(&dta);

   if ( dta.a != createcomplexl(10.0l,10.0l) ) exit(169);

/* Test 14 */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt14(&dtb);

   if ( dtb.a != createcomplexl(10.0l,10.0l) ) exit(173);
   if ( dtb.d0.a != createcomplexl(10.0l,10.0l) ) exit(177);

/* Test 15 */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   ret = fnt15(&dtc);

   if ( dtc.a != createcomplexl(10.0l,10.0l) ) exit(181);
   if ( dtc.d1.a != createcomplexl(10.0l,10.0l) ) exit(185);
   if ( dtc.d1.d0.a != createcomplexl(10.0l,10.0l) ) exit(189);

   return 0;

}
