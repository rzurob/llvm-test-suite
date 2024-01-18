
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
      if ( dta.a[i] != createcomplexf((float)(i+2),(float)(i+2)) ) exit(23);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != createcomplex((double)(i+2),(double)(i+2)) ) exit(27);
   }


/* Test 2 */

   initdts0(&dta);

   ret = fnt2(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(31);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(35);
   }


/* Test 3 */

   initdts1(&dtb);

   ret = fnt3(&dtb);

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

   ret = fnt4(dtb);

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

   ret = fnt5(&dtc);

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

   ret = fnt6(dtc);

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

   ret = fnt7(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(119);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(123);
   }


/* Test 8 */

   initdts0(&dta);

   ret = fnt8(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(127);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(131);
   }


/* Test 9 */

   initdts1(&dtb);

   ret = fnt9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(135);
      if ( dtb.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(141);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(145);
      if ( dtb.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(149);
   }

/* Test 10 */

   initdts1(&dtb);

   ret = fnt10(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(153);
      if ( dtb.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(157);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(161);
      if ( dtb.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(165);
   }

/* Test 11 */

   initdts2(&dtc);

   ret = fnt11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(169);
      if ( dtc.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(173);
      if ( dtc.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(177);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(181);
      if ( dtc.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(185);
      if ( dtc.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(189);
   }

/* Test 12 */

   initdts2(&dtc);

   ret = fnt12(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(193);
      if ( dtc.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(197);
      if ( dtc.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(201);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(205);
      if ( dtc.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(209);
      if ( dtc.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(213);
   }

/* Test 13 */

   initdts0(&dta);

   ret = fnt13(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != createcomplexf((float)(i+2),(float)(i+2)) ) exit(217);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != createcomplex((double)(i+2),(double)(i+2)) ) exit(221);
   }


/* Test 14 */

   initdts1(&dtb);

   ret = fnt14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(225);
      if ( dtb.d0.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(229);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(233);
      if ( dtb.d0.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(237);
   }

/* Test 15 */

   initdts2(&dtc);

   ret = fnt15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(241);
      if ( dtc.d1.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(245);
      if ( dtc.d1.d0.a[i] != createcomplexf((float)((i+1)*2),(float)((i+1)*2)) ) exit(249);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(255);
      if ( dtc.d1.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(259);
      if ( dtc.d1.d0.b[i] != createcomplex((double)((i+1)*2),(double)((i+1)*2)) ) exit(263);
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
