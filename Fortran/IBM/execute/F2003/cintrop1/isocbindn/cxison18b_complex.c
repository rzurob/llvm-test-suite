
/*
C code for testcase "fxison18b.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

struct dtd0 {
   float _Complex a[5][10];
   double _Complex b[3][6];
};

struct dtd1 {
   float _Complex a[5][10];
   double _Complex b[3][6];
   struct dtd0 d0;
};

struct dtd2 {
   float _Complex a[5][10];
   double _Complex b[3][6];
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
         if ( dta.a[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(21);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(25);
      }
   }

/* Test 2 */

   initdtd0(&dta);

   ret = fnt2(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(29);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(33);
      }
   }

/* Test 3 */

   initdtd1(&dtb);

   ret = fnt3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(37);
         if ( dtb.d0.a[i][j] != (float)((i+j+2)*2-1)+I*(float)((i+j+2)*2-1) ) exit(41);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(45);
         if ( dtb.d0.b[i][j] != (double)((i+j+2)*2-1)+I*(double)((i+j+2)*2-1) ) exit(49);
      }
   }

/* Test 4 */

   initdtd1(&dtb);

   ret = fnt4(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(53);
         if ( dtb.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(57);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(61);
         if ( dtb.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(65);
      }
   }

/* Test 5 */

   initdtd2(&dtc);

   ret = fnt5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(69);
         if ( dtc.d1.a[i][j] != (float)((i+j+2)*2-1)+I*(float)((i+j+2)*2-1) ) exit(73);
         if ( dtc.d1.d0.a[i][j] != (float)((i+j+2)*2-1)+I*(float)((i+j+2)*2-1) ) exit(77);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(81);
         if ( dtc.d1.b[i][j] != (double)((i+j+2)*2-1)+I*(double)((i+j+2)*2-1) ) exit(85);
         if ( dtc.d1.d0.b[i][j] != (double)((i+j+2)*2-1)+I*(double)((i+j+2)*2-1) ) exit(89);
      }
   }

/* Test 6 */

   initdtd2(&dtc);

   ret = fnt6(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(93);
         if ( dtc.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(97);
         if ( dtc.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(101);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(105);
         if ( dtc.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(109);
         if ( dtc.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(113);
      }
   }

/* Test 7 */

   initdtd0(&dta);

   ret = fnt7(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(117);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(121);
      }
   }

/* Test 7a */

   initdtd0(&dta);

   ret = fnt7a(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(125);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(129);
      }
   }

/* Test 8 */

   initdtd0(&dta);

   ret = fnt8(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(133);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(137);
      }
   }

/* Test 8a */

   initdtd0(&dta);

   ret = fnt8a(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(143);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(147);
      }
   }

/* Test 9 */

   initdtd1(&dtb);

   ret = fnt9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(151);
         if ( dtb.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(155);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(159);
         if ( dtb.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(163);
      }
   }

/* Test 9a */

   initdtd1(&dtb);

   ret = fnt9a(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(167);
         if ( dtb.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(171);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(175);
         if ( dtb.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(179);
      }
   }

/* Test 10 */

   initdtd1(&dtb);

   ret = fnt10(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(183);
         if ( dtb.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(187);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(191);
         if ( dtb.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(195);
      }
   }

/* Test 10a */

   initdtd1(&dtb);

   ret = fnt10a(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(199);
         if ( dtb.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(203);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(207);
         if ( dtb.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(211);
      }
   }

/* Test 11 */

   initdtd2(&dtc);

   ret = fnt11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(215);
         if ( dtc.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(219);
         if ( dtc.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(223);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(227);
         if ( dtc.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(231);
         if ( dtc.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(235);
      }
   }

/* Test 11a */

   initdtd2(&dtc);

   ret = fnt11a(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(239);
         if ( dtc.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(243);
         if ( dtc.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(247);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(253);
         if ( dtc.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(257);
         if ( dtc.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(261);
      }
   }

/* Test 12 */

   initdtd2(&dtc);

   ret = fnt12(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(265);
         if ( dtc.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(269);
         if ( dtc.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(273);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(277);
         if ( dtc.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(281);
         if ( dtc.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(285);
      }
   }

/* Test 12a */

   initdtd2(&dtc);

   ret = fnt12a(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(289);
         if ( dtc.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(293);
         if ( dtc.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(297);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(301);
         if ( dtc.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(305);
         if ( dtc.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(309);
      }
   }

/* Test 13 */

   initdtd0(&dta);

   ret = fnt13(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(313);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(317);
      }
   }

/* Test 14 */

   initdtd1(&dtb);

   ret = fnt14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(321);
         if ( dtb.d0.a[i][j] != (float)((i+j+2)*2-1)+I*(float)((i+j+2)*2-1) ) exit(325);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(329);
         if ( dtb.d0.b[i][j] != (double)((i+j+2)*2-1)+I*(double)((i+j+2)*2-1) ) exit(333);
      }
   }

/* Test 15 */

   initdtd2(&dtc);

   ret = fnt15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(337);
         if ( dtc.d1.a[i][j] != (float)((i+j+2)*2-1)+I*(float)((i+j+2)*2-1) ) exit(341);
         if ( dtc.d1.d0.a[i][j] != (float)((i+j+2)*2-1)+I*(float)((i+j+2)*2-1) ) exit(345);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(349);
         if ( dtc.d1.b[i][j] != (double)((i+j+2)*2-1)+I*(double)((i+j+2)*2-1) ) exit(353);
         if ( dtc.d1.d0.b[i][j] != (double)((i+j+2)*2-1)+I*(double)((i+j+2)*2-1) ) exit(357);
      }
   }

   return 0;
}

void initdtd0(struct dtd0 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         x->a[i][j] = (float)(i+j+1)+I*(float)(i+j+1);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         x->b[i][j] = (double)(i+j+1)+I*(double)(i+j+1);
      }
   }

}

void initdtd1(struct dtd1 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         x->a[i][j] = (float)(i+j+1)+I*(float)(i+j+1);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         x->b[i][j] = (double)(i+j+1)+I*(double)(i+j+1);
      }
   }

   initdtd0(&x->d0);

}

void initdtd2(struct dtd2 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         x->a[i][j] = (float)(i+j+1)+I*(float)(i+j+1);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         x->b[i][j] = (double)(i+j+1)+I*(double)(i+j+1);
      }
   }

   initdtd1(&x->d1);

}
