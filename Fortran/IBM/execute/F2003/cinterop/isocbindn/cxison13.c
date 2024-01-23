
/*
	C code for testcase "fxison12.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

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

#ifdef CMPLX
   dta.a = 5.0f+I*5.0f;
#else
   dta.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dta.b = 10.0+I*10.0;
#else
   dta.b = createcomplex(10.0,10.0);
#endif

   sub1(&dta);

#ifdef CMPLX
   if ( dta.a != 10.0f+I*10.0f ) exit(21);
#else
   if ( dta.a != createcomplexf(10.0f,10.0f) ) exit(23);
#endif
#ifdef CMPLX
   if ( dta.b != 20.0+I*20.0 ) exit(25);
#else
   if ( dta.b != createcomplex(20.0,20.0) ) exit(27);
#endif

/* Test 2 */

#ifdef CMPLX
   dta.a = 5.0f+I*5.0f;
#else
   dta.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dta.b = 10.0+I*10.0;
#else
   dta.b = createcomplex(10.0,10.0);
#endif

   sub2(dta);

#ifdef CMPLX
   if ( dta.a != 5.0f+I*5.0f ) exit(29);
#else
   if ( dta.a != createcomplexf(5.0f,5.0f) ) exit(31);
#endif
#ifdef CMPLX
   if ( dta.b != 10.0+I*10.0 ) exit(33);
#else
   if ( dta.b != createcomplex(10.0,10.0) ) exit(35);
#endif

/* Test 3 */

#ifdef CMPLX
   dtb.a = 5.0f+I*5.0f;
#else
   dtb.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.b = 10.0+I*10.0;
#else
   dtb.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0f+I*5.0f;
#else
   dtb.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.d0.b = 10.0+I*10.0;
#else
   dtb.d0.b = createcomplex(10.0,10.0);
#endif

   sub3(&dtb);

#ifdef CMPLX
   if ( dtb.a != 10.0f+I*10.0f ) exit(37);
#else
   if ( dtb.a != createcomplexf(10.0f,10.0f) ) exit(39);
#endif
#ifdef CMPLX
   if ( dtb.b != 20.0+I*20.0 ) exit(41);
#else
   if ( dtb.b != createcomplex(20.0,20.0) ) exit(43);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 10.0f+I*10.0f ) exit(45);
#else
   if ( dtb.d0.a != createcomplexf(10.0f,10.0f) ) exit(47);
#endif
#ifdef CMPLX
   if ( dtb.d0.b != 20.0+I*20.0 ) exit(49);
#else
   if ( dtb.d0.b != createcomplex(20.0,20.0) ) exit(51);
#endif

/* Test 4 */

#ifdef CMPLX
   dtb.a = 5.0f+I*5.0f;
#else
   dtb.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.b = 10.0+I*10.0;
#else
   dtb.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0f+I*5.0f;
#else
   dtb.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.d0.b = 10.0+I*10.0;
#else
   dtb.d0.b = createcomplex(10.0,10.0);
#endif

   sub4(dtb);

#ifdef CMPLX
   if ( dtb.a != 5.0f+I*5.0f ) exit(53);
#else
   if ( dtb.a != createcomplexf(5.0f,5.0f) ) exit(55);
#endif
#ifdef CMPLX
   if ( dtb.b != 10.0+I*10.0 ) exit(57);
#else
   if ( dtb.b != createcomplex(10.0,10.0) ) exit(59);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 5.0f+I*5.0f ) exit(61);
#else
   if ( dtb.d0.a != createcomplexf(5.0f,5.0f) ) exit(63);
#endif
#ifdef CMPLX
   if ( dtb.d0.b != 10.0+I*10.0 ) exit(65);
#else
   if ( dtb.d0.b != createcomplex(10.0,10.0) ) exit(67);
#endif

/* Test 5 */

#ifdef CMPLX
   dtc.a = 5.0f+I*5.0f;
#else
   dtc.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.b = 10.0+I*10.0;
#else
   dtc.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0f+I*5.0f;
#else
   dtc.d1.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.b = 10.0+I*10.0;
#else
   dtc.d1.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0f+I*5.0f;
#else
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.d0.b = 10.0+I*10.0;
#else
   dtc.d1.d0.b = createcomplex(10.0,10.0);
#endif

   sub5(&dtc);

#ifdef CMPLX
   if ( dtc.a != 10.0f+I*10.0f ) exit(69);
#else
   if ( dtc.a != createcomplexf(10.0f,10.0f) ) exit(71);
#endif
#ifdef CMPLX
   if ( dtc.b != 20.0+I*20.0 ) exit(73);
#else
   if ( dtc.b != createcomplex(20.0,20.0) ) exit(75);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 10.0f+I*10.0f ) exit(77);
#else
   if ( dtc.d1.a != createcomplexf(10.0f,10.0f) ) exit(79);
#endif
#ifdef CMPLX
   if ( dtc.d1.b != 20.0+I*20.0 ) exit(81);
#else
   if ( dtc.d1.b != createcomplex(20.0,20.0) ) exit(83);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 10.0f+I*10.0f ) exit(85);
#else
   if ( dtc.d1.d0.a != createcomplexf(10.0f,10.0f) ) exit(87);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.b != 20.0+I*20.0 ) exit(89);
#else
   if ( dtc.d1.d0.b != createcomplex(20.0,20.0) ) exit(91);
#endif

/* Test 6 */

#ifdef CMPLX
   dtc.a = 5.0f+I*5.0f;
#else
   dtc.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.b = 10.0+I*10.0;
#else
   dtc.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0f+I*5.0f;
#else
   dtc.d1.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.b = 10.0+I*10.0;
#else
   dtc.d1.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0f+I*5.0f;
#else
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.d0.b = 10.0+I*10.0;
#else
   dtc.d1.d0.b = createcomplex(10.0,10.0);
#endif

   sub6(dtc);

#ifdef CMPLX
   if ( dtc.a != 5.0f+I*5.0f ) exit(93);
#else
   if ( dtc.a != createcomplexf(5.0f,5.0f) ) exit(95);
#endif
#ifdef CMPLX
   if ( dtc.b != 10.0+I*10.0 ) exit(97);
#else
   if ( dtc.b != createcomplex(10.0,10.0) ) exit(99);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 5.0f+I*5.0f ) exit(101);
#else
   if ( dtc.d1.a != createcomplexf(5.0f,5.0f) ) exit(103);
#endif
#ifdef CMPLX
   if ( dtc.d1.b != 10.0+I*10.0 ) exit(105);
#else
   if ( dtc.d1.b != createcomplex(10.0,10.0) ) exit(107);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 5.0f+I*5.0f ) exit(109);
#else
   if ( dtc.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(111);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.b != 10.0+I*10.0 ) exit(113);
#else
   if ( dtc.d1.d0.b != createcomplex(10.0,10.0) ) exit(115);
#endif

/* Test 7 */

#ifdef CMPLX
   dta.a = 5.0f+I*5.0f;
#else
   dta.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dta.b = 10.0+I*10.0;
#else
   dta.b = createcomplex(10.0,10.0);
#endif

   sub7(&dta);

#ifdef CMPLX
   if ( dta.a != 5.0f+I*5.0f ) exit(117);
#else
   if ( dta.a != createcomplexf(5.0f,5.0f) ) exit(119);
#endif
#ifdef CMPLX
   if ( dta.b != 10.0+I*10.0 ) exit(121);
#else
   if ( dta.b != createcomplex(10.0,10.0) ) exit(123);
#endif

/* Test 7a */

#ifdef CMPLX
   dta.a = 5.0f+I*5.0f;
#else
   dta.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dta.b = 10.0+I*10.0;
#else
   dta.b = createcomplex(10.0,10.0);
#endif

   sub7a(&dta);

#ifdef CMPLX
   if ( dta.a != 5.0f+I*5.0f ) exit(125);
#else
   if ( dta.a != createcomplexf(5.0f,5.0f) ) exit(127);
#endif
#ifdef CMPLX
   if ( dta.b != 10.0+I*10.0 ) exit(129);
#else
   if ( dta.b != createcomplex(10.0,10.0) ) exit(131);
#endif

/* Test 8 */

#ifdef CMPLX
   dta.a = 5.0f+I*5.0f;
#else
   dta.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dta.b = 10.0+I*10.0;
#else
   dta.b = createcomplex(10.0,10.0);
#endif

   sub8(dta);

#ifdef CMPLX
   if ( dta.a != 5.0f+I*5.0f ) exit(133);
#else
   if ( dta.a != createcomplexf(5.0f,5.0f) ) exit(135);
#endif
#ifdef CMPLX
   if ( dta.b != 10.0+I*10.0 ) exit(137);
#else
   if ( dta.b != createcomplex(10.0,10.0) ) exit(141);
#endif

/* Test 8a */

#ifdef CMPLX
   dta.a = 5.0f+I*5.0f;
#else
   dta.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dta.b = 10.0+I*10.0;
#else
   dta.b = createcomplex(10.0,10.0);
#endif

   sub8a(dta);

#ifdef CMPLX
   if ( dta.a != 5.0f+I*5.0f ) exit(143);
#else
   if ( dta.a != createcomplexf(5.0f,5.0f) ) exit(145);
#endif
#ifdef CMPLX
   if ( dta.b != 10.0+I*10.0 ) exit(147);
#else
   if ( dta.b != createcomplex(10.0,10.0) ) exit(149);
#endif

/* Test 9 */

#ifdef CMPLX
   dtb.a = 5.0f+I*5.0f;
#else
   dtb.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.b = 10.0+I*10.0;
#else
   dtb.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0f+I*5.0f;
#else
   dtb.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.d0.b = 10.0+I*10.0;
#else
   dtb.d0.b = createcomplex(10.0,10.0);
#endif

   sub9(&dtb);

#ifdef CMPLX
   if ( dtb.a != 5.0f+I*5.0f ) exit(151);
#else
   if ( dtb.a != createcomplexf(5.0f,5.0f) ) exit(153);
#endif
#ifdef CMPLX
   if ( dtb.b != 10.0+I*10.0 ) exit(155);
#else
   if ( dtb.b != createcomplex(10.0,10.0) ) exit(157);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 5.0f+I*5.0f ) exit(159);
#else
   if ( dtb.d0.a != createcomplexf(5.0f,5.0f) ) exit(161);
#endif
#ifdef CMPLX
   if ( dtb.d0.b != 10.0+I*10.0 ) exit(163);
#else
   if ( dtb.d0.b != createcomplex(10.0,10.0) ) exit(165);
#endif

/* Test 9a */

#ifdef CMPLX
   dtb.a = 5.0f+I*5.0f;
#else
   dtb.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.b = 10.0+I*10.0;
#else
   dtb.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0f+I*5.0f;
#else
   dtb.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.d0.b = 10.0+I*10.0;
#else
   dtb.d0.b = createcomplex(10.0,10.0);
#endif

   sub9a(&dtb);

#ifdef CMPLX
   if ( dtb.a != 5.0f+I*5.0f ) exit(167);
#else
   if ( dtb.a != createcomplexf(5.0f,5.0f) ) exit(169);
#endif
#ifdef CMPLX
   if ( dtb.b != 10.0+I*10.0 ) exit(171);
#else
   if ( dtb.b != createcomplex(10.0,10.0) ) exit(173);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 5.0f+I*5.0f ) exit(175);
#else
   if ( dtb.d0.a != createcomplexf(5.0f,5.0f) ) exit(177);
#endif
#ifdef CMPLX
   if ( dtb.d0.b != 10.0+I*10.0 ) exit(179);
#else
   if ( dtb.d0.b != createcomplex(10.0,10.0) ) exit(181);
#endif

/* Test 10 */

#ifdef CMPLX
   dtb.a = 5.0f+I*5.0f;
#else
   dtb.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.b = 10.0+I*10.0;
#else
   dtb.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0f+I*5.0f;
#else
   dtb.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.d0.b = 10.0+I*10.0;
#else
   dtb.d0.b = createcomplex(10.0,10.0);
#endif

   sub10(dtb);

#ifdef CMPLX
   if ( dtb.a != 5.0f+I*5.0f ) exit(183);
#else
   if ( dtb.a != createcomplexf(5.0f,5.0f) ) exit(185);
#endif
#ifdef CMPLX
   if ( dtb.b != 10.0+I*10.0 ) exit(187);
#else
   if ( dtb.b != createcomplex(10.0,10.0) ) exit(189);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 5.0f+I*5.0f ) exit(191);
#else
   if ( dtb.d0.a != createcomplexf(5.0f,5.0f) ) exit(193);
#endif
#ifdef CMPLX
   if ( dtb.d0.b != 10.0+I*10.0 ) exit(195);
#else
   if ( dtb.d0.b != createcomplex(10.0,10.0) ) exit(197);
#endif

/* Test 10a */

#ifdef CMPLX
   dtb.a = 5.0f+I*5.0f;
#else
   dtb.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.b = 10.0+I*10.0;
#else
   dtb.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0f+I*5.0f;
#else
   dtb.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.d0.b = 10.0+I*10.0;
#else
   dtb.d0.b = createcomplex(10.0,10.0);
#endif

   sub10a(dtb);

#ifdef CMPLX
   if ( dtb.a != 5.0f+I*5.0f ) exit(199);
#else
   if ( dtb.a != createcomplexf(5.0f,5.0f) ) exit(201);
#endif
#ifdef CMPLX
   if ( dtb.b != 10.0+I*10.0 ) exit(203);
#else
   if ( dtb.b != createcomplex(10.0,10.0) ) exit(205);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 5.0f+I*5.0f ) exit(207);
#else
   if ( dtb.d0.a != createcomplexf(5.0f,5.0f) ) exit(209);
#endif
#ifdef CMPLX
   if ( dtb.d0.b != 10.0+I*10.0 ) exit(211);
#else
   if ( dtb.d0.b != createcomplex(10.0,10.0) ) exit(213);
#endif

/* Test 11 */

#ifdef CMPLX
   dtc.a = 5.0f+I*5.0f;
#else
   dtc.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.b = 10.0+I*10.0;
#else
   dtc.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0f+I*5.0f;
#else
   dtc.d1.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.b = 10.0+I*10.0;
#else
   dtc.d1.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0f+I*5.0f;
#else
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.d0.b = 10.0+I*10.0;
#else
   dtc.d1.d0.b = createcomplex(10.0,10.0);
#endif

   sub11(&dtc);

#ifdef CMPLX
   if ( dtc.a != 5.0f+I*5.0f ) exit(215);
#else
   if ( dtc.a != createcomplexf(5.0f,5.0f) ) exit(217);
#endif
#ifdef CMPLX
   if ( dtc.b != 10.0+I*10.0 ) exit(219);
#else
   if ( dtc.b != createcomplex(10.0,10.0) ) exit(221);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 5.0f+I*5.0f ) exit(223);
#else
   if ( dtc.d1.a != createcomplexf(5.0f,5.0f) ) exit(225);
#endif
#ifdef CMPLX
   if ( dtc.d1.b != 10.0+I*10.0 ) exit(227);
#else
   if ( dtc.d1.b != createcomplex(10.0,10.0) ) exit(229);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 5.0f+I*5.0f ) exit(231);
#else
   if ( dtc.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(233);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.b != 10.0+I*10.0 ) exit(235);
#else
   if ( dtc.d1.d0.b != createcomplex(10.0,10.0) ) exit(237);
#endif

/* Test 11a */

#ifdef CMPLX
   dtc.a = 5.0f+I*5.0f;
#else
   dtc.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.b = 10.0+I*10.0;
#else
   dtc.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0f+I*5.0f;
#else
   dtc.d1.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.b = 10.0+I*10.0;
#else
   dtc.d1.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0f+I*5.0f;
#else
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.d0.b = 10.0+I*10.0;
#else
   dtc.d1.d0.b = createcomplex(10.0,10.0);
#endif

   sub11a(&dtc);

#ifdef CMPLX
   if ( dtc.a != 5.0f+I*5.0f ) exit(239);
#else
   if ( dtc.a != createcomplexf(5.0f,5.0f) ) exit(241);
#endif
#ifdef CMPLX
   if ( dtc.b != 10.0+I*10.0 ) exit(243);
#else
   if ( dtc.b != createcomplex(10.0,10.0) ) exit(245);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 5.0f+I*5.0f ) exit(247);
#else
   if ( dtc.d1.a != createcomplexf(5.0f,5.0f) ) exit(249);
#endif
#ifdef CMPLX
   if ( dtc.d1.b != 10.0+I*10.0 ) exit(253);
#else
   if ( dtc.d1.b != createcomplex(10.0,10.0) ) exit(255);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 5.0f+I*5.0f ) exit(257);
#else
   if ( dtc.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(259);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.b != 10.0+I*10.0 ) exit(261);
#else
   if ( dtc.d1.d0.b != createcomplex(10.0,10.0) ) exit(263);
#endif

/* Test 12 */

#ifdef CMPLX
   dtc.a = 5.0f+I*5.0f;
#else
   dtc.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.b = 10.0+I*10.0;
#else
   dtc.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0f+I*5.0f;
#else
   dtc.d1.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.b = 10.0+I*10.0;
#else
   dtc.d1.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0f+I*5.0f;
#else
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.d0.b = 10.0+I*10.0;
#else
   dtc.d1.d0.b = createcomplex(10.0,10.0);
#endif

   sub12(dtc);

#ifdef CMPLX
   if ( dtc.a != 5.0f+I*5.0f ) exit(265);
#else
   if ( dtc.a != createcomplexf(5.0f,5.0f) ) exit(267);
#endif
#ifdef CMPLX
   if ( dtc.b != 10.0+I*10.0 ) exit(269);
#else
   if ( dtc.b != createcomplex(10.0,10.0) ) exit(271);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 5.0f+I*5.0f ) exit(273);
#else
   if ( dtc.d1.a != createcomplexf(5.0f,5.0f) ) exit(275);
#endif
#ifdef CMPLX
   if ( dtc.d1.b != 10.0+I*10.0 ) exit(277);
#else
   if ( dtc.d1.b != createcomplex(10.0,10.0) ) exit(279);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 5.0f+I*5.0f ) exit(281);
#else
   if ( dtc.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(283);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.b != 10.0+I*10.0 ) exit(285);
#else
   if ( dtc.d1.d0.b != createcomplex(10.0,10.0) ) exit(287);
#endif

/* Test 12a */

#ifdef CMPLX
   dtc.a = 5.0f+I*5.0f;
#else
   dtc.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.b = 10.0+I*10.0;
#else
   dtc.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0f+I*5.0f;
#else
   dtc.d1.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.b = 10.0+I*10.0;
#else
   dtc.d1.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0f+I*5.0f;
#else
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.d0.b = 10.0+I*10.0;
#else
   dtc.d1.d0.b = createcomplex(10.0,10.0);
#endif

   sub12a(dtc);

#ifdef CMPLX
   if ( dtc.a != 5.0f+I*5.0f ) exit(289);
#else
   if ( dtc.a != createcomplexf(5.0f,5.0f) ) exit(291);
#endif
#ifdef CMPLX
   if ( dtc.b != 10.0+I*10.0 ) exit(293);
#else
   if ( dtc.b != createcomplex(10.0,10.0) ) exit(295);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 5.0f+I*5.0f ) exit(297);
#else
   if ( dtc.d1.a != createcomplexf(5.0f,5.0f) ) exit(299);
#endif
#ifdef CMPLX
   if ( dtc.d1.b != 10.0+I*10.0 ) exit(301);
#else
   if ( dtc.d1.b != createcomplex(10.0,10.0) ) exit(303);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 5.0f+I*5.0f ) exit(305);
#else
   if ( dtc.d1.d0.a != createcomplexf(5.0f,5.0f) ) exit(307);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.b != 10.0+I*10.0 ) exit(309);
#else
   if ( dtc.d1.d0.b != createcomplex(10.0,10.0) ) exit(311);
#endif

/* Test 13 */

#ifdef CMPLX
   dta.a = 5.0f+I*5.0f;
#else
   dta.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dta.b = 10.0+I*10.0;
#else
   dta.b = createcomplex(10.0,10.0);
#endif

   sub13(&dta);

#ifdef CMPLX
   if ( dta.a != 10.0f+I*10.0f ) exit(313);
#else
   if ( dta.a != createcomplexf(10.0f,10.0f) ) exit(315);
#endif
#ifdef CMPLX
   if ( dta.b != 20.0+I*20.0 ) exit(317);
#else
   if ( dta.b != createcomplex(20.0,20.0) ) exit(319);
#endif

/* Test 14 */

#ifdef CMPLX
   dtb.a = 5.0f+I*5.0f;
#else
   dtb.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.b = 10.0+I*10.0;
#else
   dtb.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0f+I*5.0f;
#else
   dtb.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtb.d0.b = 10.0+I*10.0;
#else
   dtb.d0.b = createcomplex(10.0,10.0);
#endif

   sub14(&dtb);

#ifdef CMPLX
   if ( dtb.a != 10.0f+I*10.0f ) exit(321);
#else
   if ( dtb.a != createcomplexf(10.0f,10.0f) ) exit(323);
#endif
#ifdef CMPLX
   if ( dtb.b != 20.0+I*20.0 ) exit(325);
#else
   if ( dtb.b != createcomplex(20.0,20.0) ) exit(327);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 10.0f+I*10.0f ) exit(329);
#else
   if ( dtb.d0.a != createcomplexf(10.0f,10.0f) ) exit(331);
#endif
#ifdef CMPLX
   if ( dtb.d0.b != 20.0+I*20.0 ) exit(333);
#else
   if ( dtb.d0.b != createcomplex(20.0,20.0) ) exit(335);
#endif

/* Test 15 */

#ifdef CMPLX
   dtc.a = 5.0f+I*5.0f;
#else
   dtc.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.b = 10.0+I*10.0;
#else
   dtc.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0f+I*5.0f;
#else
   dtc.d1.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.b = 10.0+I*10.0;
#else
   dtc.d1.b = createcomplex(10.0,10.0);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0f+I*5.0f;
#else
   dtc.d1.d0.a = createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   dtc.d1.d0.b = 10.0+I*10.0;
#else
   dtc.d1.d0.b = createcomplex(10.0,10.0);
#endif

   sub15(&dtc);

#ifdef CMPLX
   if ( dtc.a != 10.0f+I*10.0f ) exit(337);
#else
   if ( dtc.a != createcomplexf(10.0f,10.0f) ) exit(339);
#endif
#ifdef CMPLX
   if ( dtc.b != 20.0+I*20.0 ) exit(341);
#else
   if ( dtc.b != createcomplex(20.0,20.0) ) exit(343);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 10.0f+I*10.0f ) exit(345);
#else
   if ( dtc.d1.a != createcomplexf(10.0f,10.0f) ) exit(347);
#endif
#ifdef CMPLX
   if ( dtc.d1.b != 20.0+I*20.0 ) exit(349);
#else
   if ( dtc.d1.b != createcomplex(20.0,20.0) ) exit(351);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 10.0f+I*10.0f ) exit(353);
#else
   if ( dtc.d1.d0.a != createcomplexf(10.0f,10.0f) ) exit(355);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.b != 20.0+I*20.0 ) exit(357);
#else
   if ( dtc.d1.d0.b != createcomplex(20.0,20.0) ) exit(359);
#endif

   return 0;

}
