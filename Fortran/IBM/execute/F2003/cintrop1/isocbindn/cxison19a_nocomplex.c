
/*
C code for testcase "fxison18a.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

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

   void sub1(struct dts0 *);
   void sub2(struct dts0);
   void sub3(struct dts1 *);
   void sub4(struct dts1);
   void sub5(struct dts2 *);
   void sub6(struct dts2);
   void sub7(struct dts0 *);
   void sub7a(const struct dts0 *);
   void sub8(struct dts0);
   void sub8a(const struct dts0);
   void sub9(struct dts1 *);
   void sub9a(const struct dts1 *);
   void sub10(struct dts1);
   void sub10a(const struct dts1);
   void sub11(struct dts2 *);
   void sub11a(const struct dts2 *);
   void sub12(struct dts2);
   void sub12a(const struct dts2);
   void sub13(struct dts0 *);
   void sub14(struct dts1 *);
   void sub15(struct dts2 *);

   struct dts0 dta;
   struct dts1 dtb;
   struct dts2 dtc;
   int i, ret;

/* Test 1 */

   initdts0(&dta);

   sub1(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexf((float)(i+2),(float)(i+2)) ) exit(23);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != createcomplex((double)(i+2),(double)(i+2)) ) exit(27);
   }


/* Test 2 */

   initdts0(&dta);

   sub2(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(31);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(35);
   }


/* Test 3 */

   initdts1(&dtb);

   sub3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(39);
      if ( dtb.d0.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(43);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(47);
      if ( dtb.d0.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(51);
   }

/* Test 4 */

   initdts1(&dtb);

   sub4(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(55);
      if ( dtb.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(59);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(63);
      if ( dtb.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(67);
   }

/* Test 5 */

   initdts2(&dtc);

   sub5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(71);
      if ( dtc.d1.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(75);
      if ( dtc.d1.d0.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(79);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(83);
      if ( dtc.d1.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(87);
      if ( dtc.d1.d0.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(91);
   }

/* Test 6 */

   initdts2(&dtc);

   sub6(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(95);
      if ( dtc.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(99);
      if ( dtc.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(103);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(107);
      if ( dtc.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(111);
      if ( dtc.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(115);
   }

/* Test 7 */

   initdts0(&dta);

   sub7(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(119);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(123);
   }


/* Test 7a */

   initdts0(&dta);

   sub7a(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(127);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(131);
   }


/* Test 8 */

   initdts0(&dta);

   sub8(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(135);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(141);
   }


/* Test 8a */

   initdts0(&dta);

   sub8a(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(145);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(149);
   }


/* Test 9 */

   initdts1(&dtb);

   sub9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(153);
      if ( dtb.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(157);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(161);
      if ( dtb.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(165);
   }

/* Test 9a */

   initdts1(&dtb);

   sub9a(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(169);
      if ( dtb.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(173);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(177);
      if ( dtb.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(181);
   }

/* Test 10 */

   initdts1(&dtb);

   sub10(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(185);
      if ( dtb.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(189);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(193);
      if ( dtb.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(197);
   }

/* Test 10a */

   initdts1(&dtb);

   sub10a(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(201);
      if ( dtb.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(205);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(209);
      if ( dtb.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(213);
   }

/* Test 11 */

   initdts2(&dtc);

   sub11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(217);
      if ( dtc.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(221);
      if ( dtc.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(225);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(229);
      if ( dtc.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(233);
      if ( dtc.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(237);
   }

/* Test 11a */

   initdts2(&dtc);

   sub11a(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(241);
      if ( dtc.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(245);
      if ( dtc.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(249);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(255);
      if ( dtc.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(259);
      if ( dtc.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(263);
   }

/* Test 12 */

   initdts2(&dtc);

   sub12(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(267);
      if ( dtc.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(271);
      if ( dtc.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(275);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(279);
      if ( dtc.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(283);
      if ( dtc.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(287);
   }

/* Test 12a */

   initdts2(&dtc);

   sub12a(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(291);
      if ( dtc.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(295);
      if ( dtc.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(299);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(303);
      if ( dtc.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(307);
      if ( dtc.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(311);
   }

/* Test 13 */

   initdts0(&dta);

   sub13(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexf((float)(i+2),(float)(i+2)) ) exit(315);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != createcomplex((double)(i+2),(double)(i+2)) ) exit(319);
   }


/* Test 14 */

   initdts1(&dtb);

   sub14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(323);
      if ( dtb.d0.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(327);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(331);
      if ( dtb.d0.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(335);
   }

/* Test 15 */

   initdts2(&dtc);

   sub15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(339);
      if ( dtc.d1.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(343);
      if ( dtc.d1.d0.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(347);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(351);
      if ( dtc.d1.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(355);
      if ( dtc.d1.d0.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(359);
   }

   return 0;
}

void initdts0(struct dts0 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = createcomplexf((float)(i+1),(float)(i+1));
   }

   for ( i = 0; i < 3; i++ ) {
      x->b[i] = createcomplexf((double)(i+1),(double)(i+1));
   }

}

void initdts1(struct dts1 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = createcomplexf((float)(i+1),(float)(i+1));
   }

   for ( i = 0; i < 3; i++ ) {
      x->b[i] = createcomplexf((double)(i+1),(double)(i+1));
   }


   initdts0(&x->d0);
}

void initdts2(struct dts2 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = createcomplexf((float)(i+1),(float)(i+1));
   }

   for ( i = 0; i < 3; i++ ) {
      x->b[i] = createcomplexf((double)(i+1),(double)(i+1));
   }


   initdts1(&x->d1);
}
