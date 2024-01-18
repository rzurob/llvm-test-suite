
/*
	C code for testcase "fxison18a.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

struct dts0 {
   float _Complex a[5];
   double _Complex b[3];
};

struct dts1 {
   float _Complex a[5];
   double _Complex b[3];
   struct dts0 d0;
};

struct dts2 {
   float _Complex a[5];
   double _Complex b[3];
   struct dts1 d1;
};

void initdts0(struct dts0 *);
void initdts1(struct dts1 *);
void initdts2(struct dts2 *);

int main() {

   int fnt1(struct dts0 *);
   int fnt2(struct dts0);
   int fnt3(struct dts1 *);
   int fnt4(struct dts1);
   int fnt5(struct dts2 *);
   int fnt6(struct dts2);
   int fnt7(struct dts0 *);
   int fnt7a(const struct dts0 *);
   int fnt8(struct dts0);
   int fnt8a(const struct dts0);
   int fnt9(struct dts1 *);
   int fnt9a(const struct dts1 *);
   int fnt10(struct dts1);
   int fnt10a(const struct dts1);
   int fnt11(struct dts2 *);
   int fnt11a(const struct dts2 *);
   int fnt12(struct dts2);
   int fnt12a(const struct dts2);
   int fnt13(struct dts0 *);
   int fnt14(struct dts1 *);
   int fnt15(struct dts2 *);

   struct dts0 dta;
   struct dts1 dtb;
   struct dts2 dtc;
   int i, ret;

/* Test 1 */

   initdts0(&dta);

   ret = fnt1(&dta);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dta.a[i] != (float)(i+2)+I*(float)(i+2) ) exit(21);
#else
      if ( dta.a[i] != createcomplexf((float)(i+2),(float)(i+2)) ) exit(23);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dta.b[i] != (double)(i+2)+I*(double)(i+2) ) exit(25);
#else
      if ( dta.b[i] != createcomplex((double)(i+2),(double)(i+2)) ) exit(27);
#endif
   }


/* Test 2 */

   initdts0(&dta);

   ret = fnt2(dta);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dta.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(29);
#else
      if ( dta.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(31);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dta.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(33);
#else
      if ( dta.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(35);
#endif
   }


/* Test 3 */

   initdts1(&dtb);

   ret = fnt3(&dtb);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtb.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(37);
#else
      if ( dtb.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(39);
#endif
#ifdef CMPLX
      if ( dtb.d0.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(41);
#else
      if ( dtb.d0.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(43);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dtb.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(45);
#else
      if ( dtb.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(47);
#endif
#ifdef CMPLX
      if ( dtb.d0.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(49);
#else
      if ( dtb.d0.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(51);
#endif
   }

/* Test 4 */

   initdts1(&dtb);

   ret = fnt4(dtb);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtb.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(53);
#else
      if ( dtb.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(55);
#endif
#ifdef CMPLX
      if ( dtb.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(57);
#else
      if ( dtb.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(59);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dtb.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(61);
#else
      if ( dtb.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(63);
#endif
#ifdef CMPLX
      if ( dtb.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(65);
#else
      if ( dtb.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(67);
#endif
   }

/* Test 5 */

   initdts2(&dtc);

   ret = fnt5(&dtc);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtc.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(69);
#else
      if ( dtc.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(71);
#endif
#ifdef CMPLX
      if ( dtc.d1.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(73);
#else
      if ( dtc.d1.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(75);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(77);
#else
      if ( dtc.d1.d0.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(79);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dtc.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(81);
#else
      if ( dtc.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(83);
#endif
#ifdef CMPLX
      if ( dtc.d1.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(85);
#else
      if ( dtc.d1.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(87);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(89);
#else
      if ( dtc.d1.d0.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(91);
#endif
   }

/* Test 6 */

   initdts2(&dtc);

   ret = fnt6(dtc);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtc.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(93);
#else
      if ( dtc.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(95);
#endif
#ifdef CMPLX
      if ( dtc.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(97);
#else
      if ( dtc.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(99);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(101);
#else
      if ( dtc.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(103);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dtc.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(105);
#else
      if ( dtc.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(107);
#endif
#ifdef CMPLX
      if ( dtc.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(109);
#else
      if ( dtc.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(111);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(113);
#else
      if ( dtc.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(115);
#endif
   }

/* Test 7 */

   initdts0(&dta);

   ret = fnt7(&dta);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dta.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(117);
#else
      if ( dta.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(119);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dta.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(121);
#else
      if ( dta.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(123);
#endif
   }


/* Test 8 */

   initdts0(&dta);

   ret = fnt8(dta);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dta.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(125);
#else
      if ( dta.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(127);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dta.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(129);
#else
      if ( dta.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(131);
#endif
   }


/* Test 9 */

   initdts1(&dtb);

   ret = fnt9(&dtb);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtb.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(133);
#else
      if ( dtb.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(135);
#endif
#ifdef CMPLX
      if ( dtb.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(137);
#else
      if ( dtb.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(141);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dtb.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(143);
#else
      if ( dtb.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(145);
#endif
#ifdef CMPLX
      if ( dtb.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(147);
#else
      if ( dtb.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(149);
#endif
   }

/* Test 10 */

   initdts1(&dtb);

   ret = fnt10(dtb);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtb.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(151);
#else
      if ( dtb.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(153);
#endif
#ifdef CMPLX
      if ( dtb.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(155);
#else
      if ( dtb.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(157);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dtb.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(159);
#else
      if ( dtb.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(161);
#endif
#ifdef CMPLX
      if ( dtb.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(163);
#else
      if ( dtb.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(165);
#endif
   }

/* Test 11 */

   initdts2(&dtc);

   ret = fnt11(&dtc);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtc.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(167);
#else
      if ( dtc.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(169);
#endif
#ifdef CMPLX
      if ( dtc.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(171);
#else
      if ( dtc.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(173);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(175);
#else
      if ( dtc.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(177);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dtc.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(179);
#else
      if ( dtc.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(181);
#endif
#ifdef CMPLX
      if ( dtc.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(183);
#else
      if ( dtc.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(185);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(187);
#else
      if ( dtc.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(189);
#endif
   }

/* Test 12 */

   initdts2(&dtc);

   ret = fnt12(dtc);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtc.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(191);
#else
      if ( dtc.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(193);
#endif
#ifdef CMPLX
      if ( dtc.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(195);
#else
      if ( dtc.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(197);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(199);
#else
      if ( dtc.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(201);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dtc.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(203);
#else
      if ( dtc.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(205);
#endif
#ifdef CMPLX
      if ( dtc.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(207);
#else
      if ( dtc.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(209);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(211);
#else
      if ( dtc.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(213);
#endif
   }

/* Test 13 */

   initdts0(&dta);

   ret = fnt13(&dta);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dta.a[i] != (float)(i+2)+I*(float)(i+2) ) exit(215);
#else
      if ( dta.a[i] != createcomplexf((float)(i+2),(float)(i+2)) ) exit(217);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dta.b[i] != (double)(i+2)+I*(double)(i+2) ) exit(219);
#else
      if ( dta.b[i] != createcomplex((double)(i+2),(double)(i+2)) ) exit(221);
#endif
   }


/* Test 14 */

   initdts1(&dtb);

   ret = fnt14(&dtb);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtb.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(223);
#else
      if ( dtb.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(225);
#endif
#ifdef CMPLX
      if ( dtb.d0.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(227);
#else
      if ( dtb.d0.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(229);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dtb.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(231);
#else
      if ( dtb.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(233);
#endif
#ifdef CMPLX
      if ( dtb.d0.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(235);
#else
      if ( dtb.d0.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(237);
#endif
   }

/* Test 15 */

   initdts2(&dtc);

   ret = fnt15(&dtc);

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dtc.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(239);
#else
      if ( dtc.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(241);
#endif
#ifdef CMPLX
      if ( dtc.d1.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(243);
#else
      if ( dtc.d1.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(245);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(247);
#else
      if ( dtc.d1.d0.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(249);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dtc.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(253);
#else
      if ( dtc.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(255);
#endif
#ifdef CMPLX
      if ( dtc.d1.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(257);
#else
      if ( dtc.d1.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(259);
#endif
#ifdef CMPLX
      if ( dtc.d1.d0.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(261);
#else
      if ( dtc.d1.d0.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(263);
#endif
   }

   return 0;
}

void initdts0(struct dts0 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      x->a[i] = (float)(i+1)+I*(float)(i+1);
#else
      x->a[i] = createcomplexf((float)(i+1),(float)(i+1));
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      x->b[i] = (double)(i+1)+I*(double)(i+1);
#else
      x->b[i] = createcomplexf((double)(i+1),(double)(i+1));
#endif
   }

}

void initdts1(struct dts1 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      x->a[i] = (float)(i+1)+I*(float)(i+1);
#else
      x->a[i] = createcomplexf((float)(i+1),(float)(i+1));
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      x->b[i] = (double)(i+1)+I*(double)(i+1);
#else
      x->b[i] = createcomplexf((double)(i+1),(double)(i+1));
#endif
   }


   initdts0(&x->d0);
}

void initdts2(struct dts2 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      x->a[i] = (float)(i+1)+I*(float)(i+1);
#else
      x->a[i] = createcomplexf((float)(i+1),(float)(i+1));
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      x->b[i] = (double)(i+1)+I*(double)(i+1);
#else
      x->b[i] = createcomplexf((double)(i+1),(double)(i+1));
#endif
   }


   initdts1(&x->d1);
}
