
/*
C code for testcase "fxisoq18a.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

struct dts0 {
   long double _Complex a[5];
};

struct dts1 {
   long double _Complex a[5];
   struct dts0 d0;
};

struct dts2 {
   long double _Complex a[5];
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
      if ( dta.a[i] != (long double)(i+2)+I*(long double)(i+2) ) exit(21);
   }


/* Test 2 */

   initdts0(&dta);

   ret = fnt2(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(25);
   }


/* Test 3 */

   initdts1(&dtb);

   ret = fnt3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(29);
      if ( dtb.d0.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(33);
   }


/* Test 4 */

   initdts1(&dtb);

   ret = fnt4(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(37);
      if ( dtb.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(41);
   }


/* Test 5 */

   initdts2(&dtc);

   ret = fnt5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(45);
      if ( dtc.d1.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(49);
      if ( dtc.d1.d0.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(53);
   }


/* Test 6 */

   initdts2(&dtc);

   ret = fnt6(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(57);
      if ( dtc.d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(61);
      if ( dtc.d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(65);
   }


/* Test 7 */

   initdts0(&dta);

   ret = fnt7(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(69);
   }


/* Test 8 */

   initdts0(&dta);

   ret = fnt8(dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(73);
   }


/* Test 9 */

   initdts1(&dtb);

   ret = fnt9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(77);
      if ( dtb.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(81);
   }


/* Test 10 */

   initdts1(&dtb);

   ret = fnt10(dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(85);
      if ( dtb.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(89);
   }


/* Test 11 */

   initdts2(&dtc);

   ret = fnt11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(93);
      if ( dtc.d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(97);
      if ( dtc.d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(101);
   }


/* Test 12 */

   initdts2(&dtc);

   ret = fnt12(dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(105);
      if ( dtc.d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(109);
      if ( dtc.d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(113);
   }


/* Test 13 */

   initdts0(&dta);

   ret = fnt13(&dta);

   for ( i = 0; i < 5; i++ ) {
      if ( dta.a[i] != (long double)(i+2)+I*(long double)(i+2) ) exit(117);
   }


/* Test 14 */

   initdts1(&dtb);

   ret = fnt14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      if ( dtb.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(121);
      if ( dtb.d0.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(125);
   }


/* Test 15 */

   initdts2(&dtc);

   ret = fnt15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      if ( dtc.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(129);
      if ( dtc.d1.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(133);
      if ( dtc.d1.d0.a[i] != (long double)((i+1)*2)+I*(long double)((i+1)*2) ) exit(137);
   }


   return 0;
}

void initdts0(struct dts0 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = (long double)(i+1)+I*(long double)(i+1);
   }

}

void initdts1(struct dts1 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = (long double)(i+1)+I*(long double)(i+1);
   }


   initdts0(&x->d0);
}

void initdts2(struct dts2 *x) {
   int i;

   for ( i = 0; i < 5; i++ ) {
      x->a[i] = (long double)(i+1)+I*(long double)(i+1);
   }


   initdts1(&x->d1);
}