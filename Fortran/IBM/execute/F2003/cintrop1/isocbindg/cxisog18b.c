
/*
	C code for testcase "fxisog18b.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

struct dtd0 {
   int_fast8_t a[5][10];
   intmax_t b[3][6];
};

struct dtd1 {
   int_fast8_t a[5][10];
   intmax_t b[3][6];
   struct dtd0 d0;
};

struct dtd2 {
   int_fast8_t a[5][10];
   intmax_t b[3][6];
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
         if ( dta.a[i][j] != i+j+2 ) exit(21);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != i+j+2 ) exit(23);
      }
   }

/* Test 2 */

   initdtd0(&dta);

   ret = fnt2(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != i+j+1 ) exit(25);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != i+j+1 ) exit(27);
      }
   }

/* Test 3 */

   initdtd1(&dtb);

   ret = fnt3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != i+j+2 ) exit(29);
         if ( dtb.d0.a[i][j] != (i+j+2)*2-1 ) exit(31);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != i+j+2 ) exit(33);
         if ( dtb.d0.b[i][j] != (i+j+2)*2-1 ) exit(35);
      }
   }

/* Test 4 */

   initdtd1(&dtb);

   ret = fnt4(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != i+j+1 ) exit(37);
         if ( dtb.d0.a[i][j] != i+j+1 ) exit(39);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != i+j+1 ) exit(41);
         if ( dtb.d0.b[i][j] != i+j+1 ) exit(43);
      }
   }

/* Test 5 */

   initdtd2(&dtc);

   ret = fnt5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != i+j+2 ) exit(45);
         if ( dtc.d1.a[i][j] != (i+j+2)*2-1 ) exit(47);
         if ( dtc.d1.d0.a[i][j] != (i+j+2)*2-1 ) exit(49);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != i+j+2 ) exit(51);
         if ( dtc.d1.b[i][j] != (i+j+2)*2-1 ) exit(53);
         if ( dtc.d1.d0.b[i][j] != (i+j+2)*2-1 ) exit(55);
      }
   }

/* Test 6 */

   initdtd2(&dtc);

   ret = fnt6(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != i+j+1 ) exit(57);
         if ( dtc.d1.a[i][j] != i+j+1 ) exit(59);
         if ( dtc.d1.d0.a[i][j] != i+j+1 ) exit(61);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != i+j+1 ) exit(63);
         if ( dtc.d1.b[i][j] != i+j+1 ) exit(65);
         if ( dtc.d1.d0.b[i][j] != i+j+1 ) exit(67);
      }
   }

/* Test 7 */

   initdtd0(&dta);

   ret = fnt7(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != i+j+1 ) exit(69);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != i+j+1 ) exit(71);
      }
   }

/* Test 7a */

   initdtd0(&dta);

   ret = fnt7a(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != i+j+1 ) exit(73);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != i+j+1 ) exit(75);
      }
   }

/* Test 8 */

   initdtd0(&dta);

   ret = fnt8(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != i+j+1 ) exit(77);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != i+j+1 ) exit(79);
      }
   }

/* Test 8a */

   initdtd0(&dta);

   ret = fnt8a(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != i+j+1 ) exit(81);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != i+j+1 ) exit(83);
      }
   }

/* Test 9 */

   initdtd1(&dtb);

   ret = fnt9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != i+j+1 ) exit(85);
         if ( dtb.d0.a[i][j] != i+j+1 ) exit(87);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != i+j+1 ) exit(89);
         if ( dtb.d0.b[i][j] != i+j+1 ) exit(91);
      }
   }

/* Test 9a */

   initdtd1(&dtb);

   ret = fnt9a(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != i+j+1 ) exit(93);
         if ( dtb.d0.a[i][j] != i+j+1 ) exit(95);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != i+j+1 ) exit(97);
         if ( dtb.d0.b[i][j] != i+j+1 ) exit(99);
      }
   }

/* Test 10 */

   initdtd1(&dtb);

   ret = fnt10(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != i+j+1 ) exit(101);
         if ( dtb.d0.a[i][j] != i+j+1 ) exit(103);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != i+j+1 ) exit(105);
         if ( dtb.d0.b[i][j] != i+j+1 ) exit(107);
      }
   }

/* Test 10a */

   initdtd1(&dtb);

   ret = fnt10a(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != i+j+1 ) exit(109);
         if ( dtb.d0.a[i][j] != i+j+1 ) exit(111);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != i+j+1 ) exit(113);
         if ( dtb.d0.b[i][j] != i+j+1 ) exit(115);
      }
   }

/* Test 11 */

   initdtd2(&dtc);

   ret = fnt11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != i+j+1 ) exit(117);
         if ( dtc.d1.a[i][j] != i+j+1 ) exit(119);
         if ( dtc.d1.d0.a[i][j] != i+j+1 ) exit(121);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != i+j+1 ) exit(123);
         if ( dtc.d1.b[i][j] != i+j+1 ) exit(125);
         if ( dtc.d1.d0.b[i][j] != i+j+1 ) exit(127);
      }
   }

/* Test 11a */

   initdtd2(&dtc);

   ret = fnt11a(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != i+j+1 ) exit(129);
         if ( dtc.d1.a[i][j] != i+j+1 ) exit(131);
         if ( dtc.d1.d0.a[i][j] != i+j+1 ) exit(133);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != i+j+1 ) exit(135);
         if ( dtc.d1.b[i][j] != i+j+1 ) exit(137);
         if ( dtc.d1.d0.b[i][j] != i+j+1 ) exit(141);
      }
   }

/* Test 12 */

   initdtd2(&dtc);

   ret = fnt12(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != i+j+1 ) exit(143);
         if ( dtc.d1.a[i][j] != i+j+1 ) exit(145);
         if ( dtc.d1.d0.a[i][j] != i+j+1 ) exit(147);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != i+j+1 ) exit(149);
         if ( dtc.d1.b[i][j] != i+j+1 ) exit(151);
         if ( dtc.d1.d0.b[i][j] != i+j+1 ) exit(153);
      }
   }

/* Test 12a */

   initdtd2(&dtc);

   ret = fnt12a(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != i+j+1 ) exit(155);
         if ( dtc.d1.a[i][j] != i+j+1 ) exit(157);
         if ( dtc.d1.d0.a[i][j] != i+j+1 ) exit(159);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != i+j+1 ) exit(161);
         if ( dtc.d1.b[i][j] != i+j+1 ) exit(163);
         if ( dtc.d1.d0.b[i][j] != i+j+1 ) exit(165);
      }
   }

/* Test 13 */

   initdtd0(&dta);

   ret = fnt13(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != i+j+2 ) exit(167);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dta.b[i][j] != i+j+2 ) exit(169);
      }
   }

/* Test 14 */

   initdtd1(&dtb);

   ret = fnt14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != i+j+2 ) exit(171);
         if ( dtb.d0.a[i][j] != (i+j+2)*2-1 ) exit(173);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtb.b[i][j] != i+j+2 ) exit(175);
         if ( dtb.d0.b[i][j] != (i+j+2)*2-1 ) exit(177);
      }
   }

/* Test 15 */

   initdtd2(&dtc);

   ret = fnt15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != i+j+2 ) exit(179);
         if ( dtc.d1.a[i][j] != (i+j+2)*2-1 ) exit(181);
         if ( dtc.d1.d0.a[i][j] != (i+j+2)*2-1 ) exit(183);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dtc.b[i][j] != i+j+2 ) exit(185);
         if ( dtc.d1.b[i][j] != (i+j+2)*2-1 ) exit(187);
         if ( dtc.d1.d0.b[i][j] != (i+j+2)*2-1 ) exit(189);
      }
   }

   return 0;
}

void initdtd0(struct dtd0 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         x->a[i][j] = i+j+1;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         x->b[i][j] = i+j+1;
      }
   }

}

void initdtd1(struct dtd1 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         x->a[i][j] = i+j+1;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         x->b[i][j] = i+j+1;
      }
   }

   initdtd0(&x->d0);

}

void initdtd2(struct dtd2 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         x->a[i][j] = i+j+1;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         x->b[i][j] = i+j+1;
      }
   }

   initdtd1(&x->d1);

}
