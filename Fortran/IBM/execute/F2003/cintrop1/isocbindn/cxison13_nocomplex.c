
/*
C code for testcase "fxison12.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

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

   dta.a = createcomplexf(5.0f,5.0f);
   dta.b = createcomplex(10.0,10.0);

   sub1(&dta);

   if ( dta.a != createcomplexf(10.0f,10.0f) ) exit(23);
   if ( dta.b != createcomplex(20.0,20.0) ) exit(27);

/* Test 2 */

   dta.a = createcomplexf(5.0f,5.0f);
   dta.b = createcomplex(10.0,10.0);

   sub2(dta);

   if ( dta.a != createcomplexf(5.0f,5.0f) ) exit(31);
   if ( dta.b != createcomplex(10.0,10.0) ) exit(35);

/* Test 3 */

   dtb.a = createcomplexf(5.0f,5.0f);
   dtb.b = createcomplex(10.0,10.0);
   dtb.d0.a = createcomplexf(5.0f,5.0f);
   dtb.d0.b = createcomplex(10.0,10.0);

   sub3(&dtb);

   if ( dtb.a != createcomplexf(10.0f,10.0f) ) exit(39);
   if ( dtb.b != createcomplex(20.0,20.0) ) exit(43);
   if ( dtb.d0.a != createcomplexf(10.0f,10.0f) ) exit(47);
   if ( dtb.d0.b != createcomplex(20.0,20.0) ) exit(51);

/* Test 4 */

   dtb.a = createcomplexf(5.0f,5.0f);
   dtb.b = createcomplex(10.0,10.0);
   dtb.d0.a = createcomplexf(5.0f,5.0f);
   dtb.d0.b = createcomplex(10.0,10.0);

   sub4(dtb);

   if ( dtb.a != createcomplexf(5.0f,5.0f) ) exit(55);
   if ( dtb.b != createcomplex(10.0,10.0) ) exit(59);
   if ( dtb.d0.a != createcomplexf(5.0f,5.0f) ) exit(63);
   if ( dtb.d0.b != createcomplex(10.0,10.0) ) exit(67);

/* Test 5 */

   dtc.a = createcomplexf(5.0f,5.0f);
   dtc.b = createcomplex(10.0,10.0);
   dtc.d1.a = createcomplexf(5.0f,5.0f);
   dtc.d1.b = createcomplex(10.0,10.0);
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
   dtc.d1.d0.b = createcomplex(10.0,10.0);

   sub5(&dtc);

   if ( dtc.a != createcomplexf(10.0f,10.0f) ) exit(71);
   if ( dtc.b != createcomplex(20.0,20.0) ) exit(75);
   if ( dtc.d1.a != createcomplexf(10.0f,10.0f) ) exit(79);
   if ( dtc.d1.b != createcomplex(20.0,20.0) ) exit(83);
   if ( dtc.d1.d0.a != createcomplexf(10.0f,10.0f) ) exit(87);
   if ( dtc.d1.d0.b != createcomplex(20.0,20.0) ) exit(91);

/* Test 6 */

   dtc.a = createcomplexf(5.0f,5.0f);
   dtc.b = createcomplex(10.0,10.0);
   dtc.d1.a = createcomplexf(5.0f,5.0f);
   dtc.d1.b = createcomplex(10.0,10.0);
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
   dtc.d1.d0.b = createcomplex(10.0,10.0);

   sub6(dtc);

   if ( dtc.a != createcomplexf(5.0f,5.0f) ) exit(95);
   if ( dtc.b != createcomplex(10.0,10.0) ) exit(99);
   if ( dtc.d1.a != createcomplexf(5.0f,5.0f) ) exit(103);
   if ( dtc.d1.b != createcomplex(10.0,10.0) ) exit(107);
   if ( dtc.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(111);
   if ( dtc.d1.d0.b != createcomplex(10.0,10.0) ) exit(115);

/* Test 7 */

   dta.a = createcomplexf(5.0f,5.0f);
   dta.b = createcomplex(10.0,10.0);

   sub7(&dta);

   if ( dta.a != createcomplexf(5.0f,5.0f) ) exit(119);
   if ( dta.b != createcomplex(10.0,10.0) ) exit(123);

/* Test 7a */

   dta.a = createcomplexf(5.0f,5.0f);
   dta.b = createcomplex(10.0,10.0);

   sub7a(&dta);

   if ( dta.a != createcomplexf(5.0f,5.0f) ) exit(127);
   if ( dta.b != createcomplex(10.0,10.0) ) exit(131);

/* Test 8 */

   dta.a = createcomplexf(5.0f,5.0f);
   dta.b = createcomplex(10.0,10.0);

   sub8(dta);

   if ( dta.a != createcomplexf(5.0f,5.0f) ) exit(135);
   if ( dta.b != createcomplex(10.0,10.0) ) exit(141);

/* Test 8a */

   dta.a = createcomplexf(5.0f,5.0f);
   dta.b = createcomplex(10.0,10.0);

   sub8a(dta);

   if ( dta.a != createcomplexf(5.0f,5.0f) ) exit(145);
   if ( dta.b != createcomplex(10.0,10.0) ) exit(149);

/* Test 9 */

   dtb.a = createcomplexf(5.0f,5.0f);
   dtb.b = createcomplex(10.0,10.0);
   dtb.d0.a = createcomplexf(5.0f,5.0f);
   dtb.d0.b = createcomplex(10.0,10.0);

   sub9(&dtb);

   if ( dtb.a != createcomplexf(5.0f,5.0f) ) exit(153);
   if ( dtb.b != createcomplex(10.0,10.0) ) exit(157);
   if ( dtb.d0.a != createcomplexf(5.0f,5.0f) ) exit(161);
   if ( dtb.d0.b != createcomplex(10.0,10.0) ) exit(165);

/* Test 9a */

   dtb.a = createcomplexf(5.0f,5.0f);
   dtb.b = createcomplex(10.0,10.0);
   dtb.d0.a = createcomplexf(5.0f,5.0f);
   dtb.d0.b = createcomplex(10.0,10.0);

   sub9a(&dtb);

   if ( dtb.a != createcomplexf(5.0f,5.0f) ) exit(169);
   if ( dtb.b != createcomplex(10.0,10.0) ) exit(173);
   if ( dtb.d0.a != createcomplexf(5.0f,5.0f) ) exit(177);
   if ( dtb.d0.b != createcomplex(10.0,10.0) ) exit(181);

/* Test 10 */

   dtb.a = createcomplexf(5.0f,5.0f);
   dtb.b = createcomplex(10.0,10.0);
   dtb.d0.a = createcomplexf(5.0f,5.0f);
   dtb.d0.b = createcomplex(10.0,10.0);

   sub10(dtb);

   if ( dtb.a != createcomplexf(5.0f,5.0f) ) exit(185);
   if ( dtb.b != createcomplex(10.0,10.0) ) exit(189);
   if ( dtb.d0.a != createcomplexf(5.0f,5.0f) ) exit(193);
   if ( dtb.d0.b != createcomplex(10.0,10.0) ) exit(197);

/* Test 10a */

   dtb.a = createcomplexf(5.0f,5.0f);
   dtb.b = createcomplex(10.0,10.0);
   dtb.d0.a = createcomplexf(5.0f,5.0f);
   dtb.d0.b = createcomplex(10.0,10.0);

   sub10a(dtb);

   if ( dtb.a != createcomplexf(5.0f,5.0f) ) exit(201);
   if ( dtb.b != createcomplex(10.0,10.0) ) exit(205);
   if ( dtb.d0.a != createcomplexf(5.0f,5.0f) ) exit(209);
   if ( dtb.d0.b != createcomplex(10.0,10.0) ) exit(213);

/* Test 11 */

   dtc.a = createcomplexf(5.0f,5.0f);
   dtc.b = createcomplex(10.0,10.0);
   dtc.d1.a = createcomplexf(5.0f,5.0f);
   dtc.d1.b = createcomplex(10.0,10.0);
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
   dtc.d1.d0.b = createcomplex(10.0,10.0);

   sub11(&dtc);

   if ( dtc.a != createcomplexf(5.0f,5.0f) ) exit(217);
   if ( dtc.b != createcomplex(10.0,10.0) ) exit(221);
   if ( dtc.d1.a != createcomplexf(5.0f,5.0f) ) exit(225);
   if ( dtc.d1.b != createcomplex(10.0,10.0) ) exit(229);
   if ( dtc.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(233);
   if ( dtc.d1.d0.b != createcomplex(10.0,10.0) ) exit(237);

/* Test 11a */

   dtc.a = createcomplexf(5.0f,5.0f);
   dtc.b = createcomplex(10.0,10.0);
   dtc.d1.a = createcomplexf(5.0f,5.0f);
   dtc.d1.b = createcomplex(10.0,10.0);
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
   dtc.d1.d0.b = createcomplex(10.0,10.0);

   sub11a(&dtc);

   if ( dtc.a != createcomplexf(5.0f,5.0f) ) exit(241);
   if ( dtc.b != createcomplex(10.0,10.0) ) exit(245);
   if ( dtc.d1.a != createcomplexf(5.0f,5.0f) ) exit(249);
   if ( dtc.d1.b != createcomplex(10.0,10.0) ) exit(255);
   if ( dtc.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(259);
   if ( dtc.d1.d0.b != createcomplex(10.0,10.0) ) exit(263);

/* Test 12 */

   dtc.a = createcomplexf(5.0f,5.0f);
   dtc.b = createcomplex(10.0,10.0);
   dtc.d1.a = createcomplexf(5.0f,5.0f);
   dtc.d1.b = createcomplex(10.0,10.0);
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
   dtc.d1.d0.b = createcomplex(10.0,10.0);

   sub12(dtc);

   if ( dtc.a != createcomplexf(5.0f,5.0f) ) exit(267);
   if ( dtc.b != createcomplex(10.0,10.0) ) exit(271);
   if ( dtc.d1.a != createcomplexf(5.0f,5.0f) ) exit(275);
   if ( dtc.d1.b != createcomplex(10.0,10.0) ) exit(279);
   if ( dtc.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(283);
   if ( dtc.d1.d0.b != createcomplex(10.0,10.0) ) exit(287);

/* Test 12a */

   dtc.a = createcomplexf(5.0f,5.0f);
   dtc.b = createcomplex(10.0,10.0);
   dtc.d1.a = createcomplexf(5.0f,5.0f);
   dtc.d1.b = createcomplex(10.0,10.0);
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
   dtc.d1.d0.b = createcomplex(10.0,10.0);

   sub12a(dtc);

   if ( dtc.a != createcomplexf(5.0f,5.0f) ) exit(291);
   if ( dtc.b != createcomplex(10.0,10.0) ) exit(295);
   if ( dtc.d1.a != createcomplexf(5.0f,5.0f) ) exit(299);
   if ( dtc.d1.b != createcomplex(10.0,10.0) ) exit(303);
   if ( dtc.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(307);
   if ( dtc.d1.d0.b != createcomplex(10.0,10.0) ) exit(311);

/* Test 13 */

   dta.a = createcomplexf(5.0f,5.0f);
   dta.b = createcomplex(10.0,10.0);

   sub13(&dta);

   if ( dta.a != createcomplexf(10.0f,10.0f) ) exit(315);
   if ( dta.b != createcomplex(20.0,20.0) ) exit(319);

/* Test 14 */

   dtb.a = createcomplexf(5.0f,5.0f);
   dtb.b = createcomplex(10.0,10.0);
   dtb.d0.a = createcomplexf(5.0f,5.0f);
   dtb.d0.b = createcomplex(10.0,10.0);

   sub14(&dtb);

   if ( dtb.a != createcomplexf(10.0f,10.0f) ) exit(323);
   if ( dtb.b != createcomplex(20.0,20.0) ) exit(327);
   if ( dtb.d0.a != createcomplexf(10.0f,10.0f) ) exit(331);
   if ( dtb.d0.b != createcomplex(20.0,20.0) ) exit(335);

/* Test 15 */

   dtc.a = createcomplexf(5.0f,5.0f);
   dtc.b = createcomplex(10.0,10.0);
   dtc.d1.a = createcomplexf(5.0f,5.0f);
   dtc.d1.b = createcomplex(10.0,10.0);
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
   dtc.d1.d0.b = createcomplex(10.0,10.0);

   sub15(&dtc);

   if ( dtc.a != createcomplexf(10.0f,10.0f) ) exit(339);
   if ( dtc.b != createcomplex(20.0,20.0) ) exit(343);
   if ( dtc.d1.a != createcomplexf(10.0f,10.0f) ) exit(347);
   if ( dtc.d1.b != createcomplex(20.0,20.0) ) exit(351);
   if ( dtc.d1.d0.a != createcomplexf(10.0f,10.0f) ) exit(355);
   if ( dtc.d1.d0.b != createcomplex(20.0,20.0) ) exit(359);

   return 0;

}