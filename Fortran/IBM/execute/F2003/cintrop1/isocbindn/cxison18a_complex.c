
/*
C code for testcase "fxison18a.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

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
      if ( dta.a[i] != (float)(i+2)+I*(float)(i+2) ) exit(21);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != (double)(i+2)+I*(double)(i+2) ) exit(25);
   }


/* Test 2 */

   initdts0(&dta);

   ret = fnt2(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(29);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(33);
   }


/* Test 3 */

   initdts1(&dtb);

   ret = fnt3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(37);
      if ( dtb.d0.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(41);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(45);
      if ( dtb.d0.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(49);
   }

/* Test 4 */

   initdts1(&dtb);

   ret = fnt4(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(53);
      if ( dtb.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(57);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(61);
      if ( dtb.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(65);
   }

/* Test 5 */

   initdts2(&dtc);

   ret = fnt5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(69);
      if ( dtc.d1.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(73);
      if ( dtc.d1.d0.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(77);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(81);
      if ( dtc.d1.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(85);
      if ( dtc.d1.d0.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(89);
   }

/* Test 6 */

   initdts2(&dtc);

   ret = fnt6(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(93);
      if ( dtc.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(97);
      if ( dtc.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(101);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(105);
      if ( dtc.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(109);
      if ( dtc.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(113);
   }

/* Test 7 */

   initdts0(&dta);

   ret = fnt7(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(117);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(121);
   }


/* Test 8 */

   initdts0(&dta);

   ret = fnt8(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(125);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(129);
   }


/* Test 9 */

   initdts1(&dtb);

   ret = fnt9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(133);
      if ( dtb.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(137);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(143);
      if ( dtb.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(147);
   }

/* Test 10 */

   initdts1(&dtb);

   ret = fnt10(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(151);
      if ( dtb.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(155);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(159);
      if ( dtb.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(163);
   }

/* Test 11 */

   initdts2(&dtc);

   ret = fnt11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(167);
      if ( dtc.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(171);
      if ( dtc.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(175);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(179);
      if ( dtc.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(183);
      if ( dtc.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(187);
   }

/* Test 12 */

   initdts2(&dtc);

   ret = fnt12(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(191);
      if ( dtc.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(195);
      if ( dtc.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(199);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(203);
      if ( dtc.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(207);
      if ( dtc.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(211);
   }

/* Test 13 */

   initdts0(&dta);

   ret = fnt13(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (float)(i+2)+I*(float)(i+2) ) exit(215);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dta.b[i] != (double)(i+2)+I*(double)(i+2) ) exit(219);
   }


/* Test 14 */

   initdts1(&dtb);

   ret = fnt14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(223);
      if ( dtb.d0.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(227);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtb.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(231);
      if ( dtb.d0.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(235);
   }

/* Test 15 */

   initdts2(&dtc);

   ret = fnt15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(239);
      if ( dtc.d1.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(243);
      if ( dtc.d1.d0.a[i] != (float)((i+1)*2)+I*(float)((i+1)*2) ) exit(247);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dtc.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(253);
      if ( dtc.d1.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(257);
      if ( dtc.d1.d0.b[i] != (double)((i+1)*2)+I*(double)((i+1)*2) ) exit(261);
   }

   return 0;
}

void initdts0(struct dts0 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      x->b[i] = (double)(i+1)+I*(double)(i+1);
   }

}

void initdts1(struct dts1 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      x->b[i] = (double)(i+1)+I*(double)(i+1);
   }


   initdts0(&x->d0);
}

void initdts2(struct dts2 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      x->b[i] = (double)(i+1)+I*(double)(i+1);
   }


   initdts1(&x->d1);
}
