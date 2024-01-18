
/*
	C code for testcase "fxisoo19b.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

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

   void sub1(struct dtd0 *);
   void sub2(struct dtd0);
   void sub3(struct dtd1 *);
   void sub4(struct dtd1);
   void sub5(struct dtd2 *);
   void sub6(struct dtd2);
   void sub7(struct dtd0 *);
   void sub7a(const struct dtd0 *);
   void sub8(struct dtd0);
   void sub8a(const struct dtd0);
   void sub9(struct dtd1 *);
   void sub9a(const struct dtd1 *);
   void sub10(struct dtd1);
   void sub10a(const struct dtd1);
   void sub11(struct dtd2 *);
   void sub11a(const struct dtd2 *);
   void sub12(struct dtd2);
   void sub12a(const struct dtd2);
   void sub13(struct dtd0 *);
   void sub14(struct dtd1 *);
   void sub15(struct dtd2 *);

   struct dtd0 dta;
   struct dtd1 dtb;
   struct dtd2 dtc;
   int i, j, ret;

/* Test 1 */

   initdtd0(&dta);

   sub1(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(21);
#else
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(23);
#endif
      }
   }


/* Test 2 */

   initdtd0(&dta);

   sub2(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(25);
#else
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(27);
#endif
      }
   }


/* Test 3 */

   initdtd1(&dtb);

   sub3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(29);
#else
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(31);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (long double)((i+j+2)*2-1)+I*(long double)((i+j+2)*2-1) ) exit(33);
#else
         if ( dtb.d0.a[i][j] != createcomplexl((long double)((i+j+2)*2-1),(long double)((i+j+2)*2-1)) ) exit(35);
#endif
      }
   }


/* Test 4 */

   initdtd1(&dtb);

   sub4(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(37);
#else
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(39);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(41);
#else
         if ( dtb.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(43);
#endif
      }
   }


/* Test 5 */

   initdtd2(&dtc);

   sub5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(45);
#else
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(47);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (long double)((i+j+2)*2-1)+I*(long double)((i+j+2)*2-1) ) exit(49);
#else
         if ( dtc.d1.a[i][j] != createcomplexl((long double)((i+j+2)*2-1),(long double)((i+j+2)*2-1)) ) exit(51);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (long double)((i+j+2)*2-1)+I*(long double)((i+j+2)*2-1) ) exit(53);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)((i+j+2)*2-1),(long double)((i+j+2)*2-1)) ) exit(55);
#endif
      }
   }


/* Test 6 */

   initdtd2(&dtc);

   sub6(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(57);
#else
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(59);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(61);
#else
         if ( dtc.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(63);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(65);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(67);
#endif
      }
   }


/* Test 7 */

   initdtd0(&dta);

   sub7(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(69);
#else
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(71);
#endif
      }
   }


/* Test 7a */

   initdtd0(&dta);

   sub7a(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(73);
#else
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(75);
#endif
      }
   }


/* Test 8 */

   initdtd0(&dta);

   sub8(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(77);
#else
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(79);
#endif
      }
   }


/* Test 8a */

   initdtd0(&dta);

   sub8a(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(81);
#else
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(83);
#endif
      }
   }


/* Test 9 */

   initdtd1(&dtb);

   sub9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(85);
#else
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(87);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(89);
#else
         if ( dtb.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(91);
#endif
      }
   }


/* Test 9a */

   initdtd1(&dtb);

   sub9a(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(93);
#else
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(95);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(97);
#else
         if ( dtb.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(99);
#endif
      }
   }


/* Test 10 */

   initdtd1(&dtb);

   sub10(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(101);
#else
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(103);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(105);
#else
         if ( dtb.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(107);
#endif
      }
   }


/* Test 10a */

   initdtd1(&dtb);

   sub10a(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(109);
#else
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(111);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(113);
#else
         if ( dtb.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(115);
#endif
      }
   }


/* Test 11 */

   initdtd2(&dtc);

   sub11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(117);
#else
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(119);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(121);
#else
         if ( dtc.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(123);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(125);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(127);
#endif
      }
   }


/* Test 11a */

   initdtd2(&dtc);

   sub11a(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(129);
#else
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(131);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(133);
#else
         if ( dtc.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(135);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(137);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(141);
#endif
      }
   }


/* Test 12 */

   initdtd2(&dtc);

   sub12(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(143);
#else
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(145);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(147);
#else
         if ( dtc.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(149);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(151);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(153);
#endif
      }
   }


/* Test 12a */

   initdtd2(&dtc);

   sub12a(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(155);
#else
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(157);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(159);
#else
         if ( dtc.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(161);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(163);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(165);
#endif
      }
   }


/* Test 13 */

   initdtd0(&dta);

   sub13(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(167);
#else
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(169);
#endif
      }
   }


/* Test 14 */

   initdtd1(&dtb);

   sub14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(171);
#else
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(173);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (long double)((i+j+2)*2-1)+I*(long double)((i+j+2)*2-1) ) exit(175);
#else
         if ( dtb.d0.a[i][j] != createcomplexl((long double)((i+j+2)*2-1),(long double)((i+j+2)*2-1)) ) exit(177);
#endif
      }
   }


/* Test 15 */

   initdtd2(&dtc);

   sub15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (long double)(i+j+2)+I*(long double)(i+j+2) ) exit(179);
#else
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(181);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (long double)((i+j+2)*2-1)+I*(long double)((i+j+2)*2-1) ) exit(183);
#else
         if ( dtc.d1.a[i][j] != createcomplexl((long double)((i+j+2)*2-1),(long double)((i+j+2)*2-1)) ) exit(185);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (long double)((i+j+2)*2-1)+I*(long double)((i+j+2)*2-1) ) exit(187);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)((i+j+2)*2-1),(long double)((i+j+2)*2-1)) ) exit(189);
#endif
      }
   }


   return 0;
}

void initdtd0(struct dtd0 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         x->a[i][j] = (long double)(i+j+1)+I*(long double)(i+j+1);
#else
         x->a[i][j] = createcomplexl((long double)(i+j+1),(long double)(i+j+1));
#endif
      }
   }


}

void initdtd1(struct dtd1 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         x->a[i][j] = (long double)(i+j+1)+I*(long double)(i+j+1);
#else
         x->a[i][j] = createcomplexl((long double)(i+j+1),(long double)(i+j+1));
#endif
      }
   }


   initdtd0(&x->d0);

}

void initdtd2(struct dtd2 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         x->a[i][j] = (long double)(i+j+1)+I*(long double)(i+j+1);
#else
         x->a[i][j] = createcomplexl((long double)(i+j+1),(long double)(i+j+1));
#endif
      }
   }


   initdtd1(&x->d1);

}
