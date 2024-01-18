
/*
	C code for testcase "fxisoq12.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

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

#ifdef CMPLX
   dta.a = 5.0l+I*5.0l;
#else
   dta.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt1(&dta);

#ifdef CMPLX
   if ( dta.a != 10.0l+I*10.0l ) exit(21);
#else
   if ( dta.a != createcomplexl(10.0l,10.0l) ) exit(23);
#endif

/* Test 2 */

#ifdef CMPLX
   dta.a = 5.0l+I*5.0l;
#else
   dta.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt2(dta);

#ifdef CMPLX
   if ( dta.a != 5.0l+I*5.0l ) exit(25);
#else
   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(27);
#endif

/* Test 3 */

#ifdef CMPLX
   dtb.a = 5.0l+I*5.0l;
#else
   dtb.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0l+I*5.0l;
#else
   dtb.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt3(&dtb);

#ifdef CMPLX
   if ( dtb.a != 10.0l+I*10.0l ) exit(29);
#else
   if ( dtb.a != createcomplexl(10.0l,10.0l) ) exit(31);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 10.0l+I*10.0l ) exit(33);
#else
   if ( dtb.d0.a != createcomplexl(10.0l,10.0l) ) exit(35);
#endif

/* Test 4 */

#ifdef CMPLX
   dtb.a = 5.0l+I*5.0l;
#else
   dtb.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0l+I*5.0l;
#else
   dtb.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt4(dtb);

#ifdef CMPLX
   if ( dtb.a != 5.0l+I*5.0l ) exit(37);
#else
   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(39);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(41);
#else
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(43);
#endif

/* Test 5 */

#ifdef CMPLX
   dtc.a = 5.0l+I*5.0l;
#else
   dtc.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0l+I*5.0l;
#else
   dtc.d1.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0l+I*5.0l;
#else
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt5(&dtc);

#ifdef CMPLX
   if ( dtc.a != 10.0l+I*10.0l ) exit(45);
#else
   if ( dtc.a != createcomplexl(10.0l,10.0l) ) exit(47);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 10.0l+I*10.0l ) exit(49);
#else
   if ( dtc.d1.a != createcomplexl(10.0l,10.0l) ) exit(51);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 10.0l+I*10.0l ) exit(53);
#else
   if ( dtc.d1.d0.a != createcomplexl(10.0l,10.0l) ) exit(55);
#endif

/* Test 6 */

#ifdef CMPLX
   dtc.a = 5.0l+I*5.0l;
#else
   dtc.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0l+I*5.0l;
#else
   dtc.d1.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0l+I*5.0l;
#else
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt6(dtc);

#ifdef CMPLX
   if ( dtc.a != 5.0l+I*5.0l ) exit(57);
#else
   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(59);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(61);
#else
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(63);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(65);
#else
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(67);
#endif

/* Test 7 */

#ifdef CMPLX
   dta.a = 5.0l+I*5.0l;
#else
   dta.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt7(&dta);

#ifdef CMPLX
   if ( dta.a != 5.0l+I*5.0l ) exit(69);
#else
   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(71);
#endif

/* Test 7a */

#ifdef CMPLX
   dta.a = 5.0l+I*5.0l;
#else
   dta.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt7a(&dta);

#ifdef CMPLX
   if ( dta.a != 5.0l+I*5.0l ) exit(73);
#else
   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(75);
#endif

/* Test 8 */

#ifdef CMPLX
   dta.a = 5.0l+I*5.0l;
#else
   dta.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt8(dta);

#ifdef CMPLX
   if ( dta.a != 5.0l+I*5.0l ) exit(77);
#else
   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(79);
#endif

/* Test 8a */

#ifdef CMPLX
   dta.a = 5.0l+I*5.0l;
#else
   dta.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt8a(dta);

#ifdef CMPLX
   if ( dta.a != 5.0l+I*5.0l ) exit(81);
#else
   if ( dta.a != createcomplexl(5.0l,5.0l) ) exit(83);
#endif

/* Test 9 */

#ifdef CMPLX
   dtb.a = 5.0l+I*5.0l;
#else
   dtb.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0l+I*5.0l;
#else
   dtb.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt9(&dtb);

#ifdef CMPLX
   if ( dtb.a != 5.0l+I*5.0l ) exit(85);
#else
   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(87);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(89);
#else
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(91);
#endif

/* Test 9a */

#ifdef CMPLX
   dtb.a = 5.0l+I*5.0l;
#else
   dtb.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0l+I*5.0l;
#else
   dtb.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt9a(&dtb);

#ifdef CMPLX
   if ( dtb.a != 5.0l+I*5.0l ) exit(93);
#else
   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(95);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(97);
#else
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(99);
#endif

/* Test 10 */

#ifdef CMPLX
   dtb.a = 5.0l+I*5.0l;
#else
   dtb.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0l+I*5.0l;
#else
   dtb.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt10(dtb);

#ifdef CMPLX
   if ( dtb.a != 5.0l+I*5.0l ) exit(101);
#else
   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(103);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(105);
#else
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(107);
#endif

/* Test 10a */

#ifdef CMPLX
   dtb.a = 5.0l+I*5.0l;
#else
   dtb.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0l+I*5.0l;
#else
   dtb.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt10a(dtb);

#ifdef CMPLX
   if ( dtb.a != 5.0l+I*5.0l ) exit(109);
#else
   if ( dtb.a != createcomplexl(5.0l,5.0l) ) exit(111);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 5.0l+I*5.0l ) exit(113);
#else
   if ( dtb.d0.a != createcomplexl(5.0l,5.0l) ) exit(115);
#endif

/* Test 11 */

#ifdef CMPLX
   dtc.a = 5.0l+I*5.0l;
#else
   dtc.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0l+I*5.0l;
#else
   dtc.d1.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0l+I*5.0l;
#else
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt11(&dtc);

#ifdef CMPLX
   if ( dtc.a != 5.0l+I*5.0l ) exit(117);
#else
   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(119);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(121);
#else
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(123);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(125);
#else
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(127);
#endif

/* Test 11a */

#ifdef CMPLX
   dtc.a = 5.0l+I*5.0l;
#else
   dtc.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0l+I*5.0l;
#else
   dtc.d1.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0l+I*5.0l;
#else
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt11a(&dtc);

#ifdef CMPLX
   if ( dtc.a != 5.0l+I*5.0l ) exit(129);
#else
   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(131);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(133);
#else
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(135);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(137);
#else
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(141);
#endif

/* Test 12 */

#ifdef CMPLX
   dtc.a = 5.0l+I*5.0l;
#else
   dtc.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0l+I*5.0l;
#else
   dtc.d1.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0l+I*5.0l;
#else
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt12(dtc);

#ifdef CMPLX
   if ( dtc.a != 5.0l+I*5.0l ) exit(143);
#else
   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(145);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(147);
#else
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(149);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(151);
#else
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(153);
#endif

/* Test 12a */

#ifdef CMPLX
   dtc.a = 5.0l+I*5.0l;
#else
   dtc.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0l+I*5.0l;
#else
   dtc.d1.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0l+I*5.0l;
#else
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt12a(dtc);

#ifdef CMPLX
   if ( dtc.a != 5.0l+I*5.0l ) exit(155);
#else
   if ( dtc.a != createcomplexl(5.0l,5.0l) ) exit(157);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 5.0l+I*5.0l ) exit(159);
#else
   if ( dtc.d1.a != createcomplexl(5.0l,5.0l) ) exit(161);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 5.0l+I*5.0l ) exit(163);
#else
   if ( dtc.d1.d0.a != createcomplexl(5.0l,5.0l) ) exit(165);
#endif

/* Test 13 */

#ifdef CMPLX
   dta.a = 5.0l+I*5.0l;
#else
   dta.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt13(&dta);

#ifdef CMPLX
   if ( dta.a != 10.0l+I*10.0l ) exit(167);
#else
   if ( dta.a != createcomplexl(10.0l,10.0l) ) exit(169);
#endif

/* Test 14 */

#ifdef CMPLX
   dtb.a = 5.0l+I*5.0l;
#else
   dtb.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtb.d0.a = 5.0l+I*5.0l;
#else
   dtb.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt14(&dtb);

#ifdef CMPLX
   if ( dtb.a != 10.0l+I*10.0l ) exit(171);
#else
   if ( dtb.a != createcomplexl(10.0l,10.0l) ) exit(173);
#endif
#ifdef CMPLX
   if ( dtb.d0.a != 10.0l+I*10.0l ) exit(175);
#else
   if ( dtb.d0.a != createcomplexl(10.0l,10.0l) ) exit(177);
#endif

/* Test 15 */

#ifdef CMPLX
   dtc.a = 5.0l+I*5.0l;
#else
   dtc.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.a = 5.0l+I*5.0l;
#else
   dtc.d1.a = createcomplexl(5.0l,5.0l);
#endif
#ifdef CMPLX
   dtc.d1.d0.a = 5.0l+I*5.0l;
#else
   dtc.d1.d0.a = createcomplexl(5.0l,5.0l);
#endif

   ret = fnt15(&dtc);

#ifdef CMPLX
   if ( dtc.a != 10.0l+I*10.0l ) exit(179);
#else
   if ( dtc.a != createcomplexl(10.0l,10.0l) ) exit(181);
#endif
#ifdef CMPLX
   if ( dtc.d1.a != 10.0l+I*10.0l ) exit(183);
#else
   if ( dtc.d1.a != createcomplexl(10.0l,10.0l) ) exit(185);
#endif
#ifdef CMPLX
   if ( dtc.d1.d0.a != 10.0l+I*10.0l ) exit(187);
#else
   if ( dtc.d1.d0.a != createcomplexl(10.0l,10.0l) ) exit(189);
#endif

   return 0;

}
