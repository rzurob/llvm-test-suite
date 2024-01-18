
/*
C code for testcase "fxisoo18b.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

struct dtd0 {
   long double _Complex a[5][10];
};

struct dtd1 {
   long double _Complex a[5][10];
   struct dtd0 d0;
};

struct dtd2 {
   long double _Complex a[5][10];
   struct dtd1 d1;
};

void initdtd0(struct dtd0 *);
void initdtd1(struct dtd1 *);
void initdtd2(struct dtd2 *);

int main() {

   int fnt1(struct dtd0 *);
   int fnt2(struct dtd0);
   int fnt3(struct dtd1 *);
   int fnt4(struct dtd1);
   int fnt5(struct dtd2 *);
   int fnt6(struct dtd2);
   int fnt7(struct dtd0 *);
   int fnt7a(const struct dtd0 *);
   int fnt8(struct dtd0);
   int fnt8a(const struct dtd0);
   int fnt9(struct dtd1 *);
   int fnt9a(const struct dtd1 *);
   int fnt10(struct dtd1);
   int fnt10a(const struct dtd1);
   int fnt11(struct dtd2 *);
   int fnt11a(const struct dtd2 *);
   int fnt12(struct dtd2);
   int fnt12a(const struct dtd2);
   int fnt13(struct dtd0 *);
   int fnt14(struct dtd1 *);
   int fnt15(struct dtd2 *);

   struct dtd0 dta;
   struct dtd1 dtb;
   struct dtd2 dtc;
   int i, j, ret;

/* Test 1 */

   initdtd0(&dta);

   ret = fnt1(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(21);
      }
   }


/* Test 2 */

   initdtd0(&dta);

   ret = fnt2(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(25);
      }
   }


/* Test 3 */

   initdtd1(&dtb);

   ret = fnt3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(29);
         if ( dtb.d0.a[i][j] != (long double)((i+j+2)*2-1)+I*(long double)((i+j+2)*2-1) ) exit(33);
      }
   }


/* Test 4 */

   initdtd1(&dtb);

   ret = fnt4(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(37);
         if ( dtb.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(41);
      }
   }


/* Test 5 */

   initdtd2(&dtc);

   ret = fnt5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(45);
         if ( dtc.d1.a[i][j] != (long double)((i+j+2)*2-1)+I*(long double)((i+j+2)*2-1) ) exit(49);
         if ( dtc.d1.d0.a[i][j] != (long double)((i+j+2)*2-1)+I*(long double)((i+j+2)*2-1) ) exit(53);
      }
   }


/* Test 6 */

   initdtd2(&dtc);

   ret = fnt6(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(57);
         if ( dtc.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(61);
         if ( dtc.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(65);
      }
   }


/* Test 7 */

   initdtd0(&dta);

   ret = fnt7(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(69);
      }
   }


/* Test 7a */

   initdtd0(&dta);

   ret = fnt7a(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(73);
      }
   }


/* Test 8 */

   initdtd0(&dta);

   ret = fnt8(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(77);
      }
   }


/* Test 8a */

   initdtd0(&dta);

   ret = fnt8a(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(81);
      }
   }


/* Test 9 */

   initdtd1(&dtb);

   ret = fnt9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(85);
         if ( dtb.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(89);
      }
   }


/* Test 9a */

   initdtd1(&dtb);

   ret = fnt9a(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(93);
         if ( dtb.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(97);
      }
   }


/* Test 10 */

   initdtd1(&dtb);

   ret = fnt10(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(101);
         if ( dtb.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(105);
      }
   }


/* Test 10a */

   initdtd1(&dtb);

   ret = fnt10a(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(109);
         if ( dtb.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(113);
      }
   }


/* Test 11 */

   initdtd2(&dtc);

   ret = fnt11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(117);
         if ( dtc.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(121);
         if ( dtc.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(125);
      }
   }


/* Test 11a */

   initdtd2(&dtc);

   ret = fnt11a(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(129);
         if ( dtc.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(133);
         if ( dtc.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(137);
      }
   }


/* Test 12 */

   initdtd2(&dtc);

   ret = fnt12(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(143);
         if ( dtc.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(147);
         if ( dtc.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(151);
      }
   }


/* Test 12a */

   initdtd2(&dtc);

   ret = fnt12a(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(155);
         if ( dtc.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(159);
         if ( dtc.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(163);
      }
   }


/* Test 13 */

   initdtd0(&dta);

   ret = fnt13(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(167);
      }
   }


/* Test 14 */

   initdtd1(&dtb);

   ret = fnt14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(171);
         if ( dtb.d0.a[i][j] != (long double)((i+j+2)*2-1)+I*(long double)((i+j+2)*2-1) ) exit(175);
      }
   }


/* Test 15 */

   initdtd2(&dtc);

   ret = fnt15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(179);
         if ( dtc.d1.a[i][j] != (long double)((i+j+2)*2-1)+I*(long double)((i+j+2)*2-1) ) exit(183);
         if ( dtc.d1.d0.a[i][j] != (long double)((i+j+2)*2-1)+I*(long double)((i+j+2)*2-1) ) exit(187);
      }
   }


   return 0;
}

void initdtd0(struct dtd0 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         x->a[i][j] = (long double)(i+j+1)+I*(long double)(i+j+1);
      }
   }


}

void initdtd1(struct dtd1 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         x->a[i][j] = (long double)(i+j+1)+I*(long double)(i+j+1);
      }
   }


   initdtd0(&x->d0);

}

void initdtd2(struct dtd2 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         x->a[i][j] = (long double)(i+j+1)+I*(long double)(i+j+1);
      }
   }


   initdtd1(&x->d1);

}
