
/*
C code for testcase "fxisoo12.f"
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

   dta.a = createcomplexl(5.0l,5.0l);

   sub1(&dta);

   if ( dta.a != createcomplexl(10.0l,10.0l) ) exit(23);

/* Test 2 */

   dta.a = createcomplexl(5.0l,5.0l);

   sub2(dta);

   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(27);

/* Test 3 */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   sub3(&dtb);

   if ( dtb.a != createcomplexl(10.0l,10.0l) ) exit(31);
   if ( dtb.d0.a != createcomplexl(10.0l,10.0l) ) exit(35);

/* Test 4 */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   sub4(dtb);

   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(39);
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(43);

/* Test 5 */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   sub5(&dtc);

   if ( dtc.a != createcomplexl(10.0l,10.0l) ) exit(47);
   if ( dtc.d1.a != createcomplexl(10.0l,10.0l) ) exit(51);
   if ( dtc.d1.d0.a != createcomplexl(10.0l,10.0l) ) exit(55);

/* Test 6 */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   sub6(dtc);

   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(59);
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(63);
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(67);

/* Test 7 */

   dta.a = createcomplexl(5.0l,5.0l);

   sub7(&dta);

   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(71);

/* Test 7a */

   dta.a = createcomplexl(5.0l,5.0l);

   sub7a(&dta);

   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(75);

/* Test 8 */

   dta.a = createcomplexl(5.0l,5.0l);

   sub8(dta);

   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(79);

/* Test 8a */

   dta.a = createcomplexl(5.0l,5.0l);

   sub8a(dta);

   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(83);

/* Test 9 */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   sub9(&dtb);

   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(87);
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(91);

/* Test 9a */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   sub9a(&dtb);

   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(95);
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(99);

/* Test 10 */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   sub10(dtb);

   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(103);
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(107);

/* Test 10a */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   sub10a(dtb);

   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(111);
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(115);

/* Test 11 */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   sub11(&dtc);

   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(119);
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(123);
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(127);

/* Test 11a */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   sub11a(&dtc);

   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(131);
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(135);
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(141);

/* Test 12 */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   sub12(dtc);

   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(145);
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(149);
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(153);

/* Test 12a */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   sub12a(dtc);

   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(157);
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(161);
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(165);

/* Test 13 */

   dta.a = createcomplexl(5.0l,5.0l);

   sub13(&dta);

   if ( dta.a != createcomplexl(10.0l,10.0l) ) exit(169);

/* Test 14 */

   dtb.a = createcomplexl(5.0l,5.0l);
   dtb.d0.a = createcomplexl(5.0l,5.0l);

   sub14(&dtb);

   if ( dtb.a != createcomplexl(10.0l,10.0l) ) exit(173);
   if ( dtb.d0.a != createcomplexl(10.0l,10.0l) ) exit(177);

/* Test 15 */

   dtc.a = createcomplexl(5.0l,5.0l);
   dtc.d1.a = createcomplexl(5.0l,5.0l);
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);

   sub15(&dtc);

   if ( dtc.a != createcomplexl(10.0l,10.0l) ) exit(181);
   if ( dtc.d1.a != createcomplexl(10.0l,10.0l) ) exit(185);
   if ( dtc.d1.d0.a != createcomplexl(10.0l,10.0l) ) exit(189);

   return 0;

}
