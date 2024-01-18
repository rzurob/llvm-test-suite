
/*
	C code for testcase "fxison18b.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

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
#ifdef CMPLX
         if ( dta.a[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(21);
#else
         if ( dta.a[i][j] != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(23);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dta.b[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(25);
#else
         if ( dta.b[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(27);
#endif
      }
   }

/* Test 2 */

   initdtd0(&dta);

   ret = fnt2(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(29);
#else
         if ( dta.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(31);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dta.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(33);
#else
         if ( dta.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(35);
#endif
      }
   }

/* Test 3 */

   initdtd1(&dtb);

   ret = fnt3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(37);
#else
         if ( dtb.a[i][j] != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(39);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (float)((i+j+2)*2-1)+I*(float)((i+j+2)*2-1) ) exit(41);
#else
         if ( dtb.d0.a[i][j] != createcomplexf((float)((i+j+2)*2-1),(float)((i+j+2)*2-1)) ) exit(43);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtb.b[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(45);
#else
         if ( dtb.b[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(47);
#endif
#ifdef CMPLX
         if ( dtb.d0.b[i][j] != (double)((i+j+2)*2-1)+I*(double)((i+j+2)*2-1) ) exit(49);
#else
         if ( dtb.d0.b[i][j] != createcomplex((double)((i+j+2)*2-1),(double)((i+j+2)*2-1)) ) exit(51);
#endif
      }
   }

/* Test 4 */

   initdtd1(&dtb);

   ret = fnt4(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(53);
#else
         if ( dtb.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(55);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(57);
#else
         if ( dtb.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(59);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtb.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(61);
#else
         if ( dtb.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(63);
#endif
#ifdef CMPLX
         if ( dtb.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(65);
#else
         if ( dtb.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(67);
#endif
      }
   }

/* Test 5 */

   initdtd2(&dtc);

   ret = fnt5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(69);
#else
         if ( dtc.a[i][j] != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(71);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (float)((i+j+2)*2-1)+I*(float)((i+j+2)*2-1) ) exit(73);
#else
         if ( dtc.d1.a[i][j] != createcomplexf((float)((i+j+2)*2-1),(float)((i+j+2)*2-1)) ) exit(75);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (float)((i+j+2)*2-1)+I*(float)((i+j+2)*2-1) ) exit(77);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexf((float)((i+j+2)*2-1),(float)((i+j+2)*2-1)) ) exit(79);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtc.b[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(81);
#else
         if ( dtc.b[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(83);
#endif
#ifdef CMPLX
         if ( dtc.d1.b[i][j] != (double)((i+j+2)*2-1)+I*(double)((i+j+2)*2-1) ) exit(85);
#else
         if ( dtc.d1.b[i][j] != createcomplex((double)((i+j+2)*2-1),(double)((i+j+2)*2-1)) ) exit(87);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.b[i][j] != (double)((i+j+2)*2-1)+I*(double)((i+j+2)*2-1) ) exit(89);
#else
         if ( dtc.d1.d0.b[i][j] != createcomplex((double)((i+j+2)*2-1),(double)((i+j+2)*2-1)) ) exit(91);
#endif
      }
   }

/* Test 6 */

   initdtd2(&dtc);

   ret = fnt6(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(93);
#else
         if ( dtc.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(95);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(97);
#else
         if ( dtc.d1.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(99);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(101);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(103);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtc.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(105);
#else
         if ( dtc.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(107);
#endif
#ifdef CMPLX
         if ( dtc.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(109);
#else
         if ( dtc.d1.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(111);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(113);
#else
         if ( dtc.d1.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(115);
#endif
      }
   }

/* Test 7 */

   initdtd0(&dta);

   ret = fnt7(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(117);
#else
         if ( dta.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(119);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dta.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(121);
#else
         if ( dta.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(123);
#endif
      }
   }

/* Test 7a */

   initdtd0(&dta);

   ret = fnt7a(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(125);
#else
         if ( dta.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(127);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dta.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(129);
#else
         if ( dta.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(131);
#endif
      }
   }

/* Test 8 */

   initdtd0(&dta);

   ret = fnt8(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(133);
#else
         if ( dta.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(135);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dta.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(137);
#else
         if ( dta.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(141);
#endif
      }
   }

/* Test 8a */

   initdtd0(&dta);

   ret = fnt8a(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(143);
#else
         if ( dta.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(145);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dta.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(147);
#else
         if ( dta.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(149);
#endif
      }
   }

/* Test 9 */

   initdtd1(&dtb);

   ret = fnt9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(151);
#else
         if ( dtb.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(153);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(155);
#else
         if ( dtb.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(157);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtb.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(159);
#else
         if ( dtb.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(161);
#endif
#ifdef CMPLX
         if ( dtb.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(163);
#else
         if ( dtb.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(165);
#endif
      }
   }

/* Test 9a */

   initdtd1(&dtb);

   ret = fnt9a(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(167);
#else
         if ( dtb.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(169);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(171);
#else
         if ( dtb.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(173);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtb.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(175);
#else
         if ( dtb.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(177);
#endif
#ifdef CMPLX
         if ( dtb.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(179);
#else
         if ( dtb.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(181);
#endif
      }
   }

/* Test 10 */

   initdtd1(&dtb);

   ret = fnt10(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(183);
#else
         if ( dtb.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(185);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(187);
#else
         if ( dtb.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(189);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtb.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(191);
#else
         if ( dtb.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(193);
#endif
#ifdef CMPLX
         if ( dtb.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(195);
#else
         if ( dtb.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(197);
#endif
      }
   }

/* Test 10a */

   initdtd1(&dtb);

   ret = fnt10a(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(199);
#else
         if ( dtb.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(201);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(203);
#else
         if ( dtb.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(205);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtb.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(207);
#else
         if ( dtb.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(209);
#endif
#ifdef CMPLX
         if ( dtb.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(211);
#else
         if ( dtb.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(213);
#endif
      }
   }

/* Test 11 */

   initdtd2(&dtc);

   ret = fnt11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(215);
#else
         if ( dtc.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(217);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(219);
#else
         if ( dtc.d1.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(221);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(223);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(225);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtc.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(227);
#else
         if ( dtc.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(229);
#endif
#ifdef CMPLX
         if ( dtc.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(231);
#else
         if ( dtc.d1.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(233);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(235);
#else
         if ( dtc.d1.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(237);
#endif
      }
   }

/* Test 11a */

   initdtd2(&dtc);

   ret = fnt11a(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(239);
#else
         if ( dtc.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(241);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(243);
#else
         if ( dtc.d1.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(245);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(247);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(249);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtc.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(253);
#else
         if ( dtc.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(255);
#endif
#ifdef CMPLX
         if ( dtc.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(257);
#else
         if ( dtc.d1.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(259);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(261);
#else
         if ( dtc.d1.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(263);
#endif
      }
   }

/* Test 12 */

   initdtd2(&dtc);

   ret = fnt12(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(265);
#else
         if ( dtc.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(267);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(269);
#else
         if ( dtc.d1.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(271);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(273);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(275);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtc.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(277);
#else
         if ( dtc.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(279);
#endif
#ifdef CMPLX
         if ( dtc.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(281);
#else
         if ( dtc.d1.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(283);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(285);
#else
         if ( dtc.d1.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(287);
#endif
      }
   }

/* Test 12a */

   initdtd2(&dtc);

   ret = fnt12a(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(289);
#else
         if ( dtc.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(291);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(293);
#else
         if ( dtc.d1.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(295);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(297);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(299);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtc.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(301);
#else
         if ( dtc.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(303);
#endif
#ifdef CMPLX
         if ( dtc.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(305);
#else
         if ( dtc.d1.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(307);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(309);
#else
         if ( dtc.d1.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(311);
#endif
      }
   }

/* Test 13 */

   initdtd0(&dta);

   ret = fnt13(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dta.a[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(313);
#else
         if ( dta.a[i][j] != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(315);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dta.b[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(317);
#else
         if ( dta.b[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(319);
#endif
      }
   }

/* Test 14 */

   initdtd1(&dtb);

   ret = fnt14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtb.a[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(321);
#else
         if ( dtb.a[i][j] != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(323);
#endif
#ifdef CMPLX
         if ( dtb.d0.a[i][j] != (float)((i+j+2)*2-1)+I*(float)((i+j+2)*2-1) ) exit(325);
#else
         if ( dtb.d0.a[i][j] != createcomplexf((float)((i+j+2)*2-1),(float)((i+j+2)*2-1)) ) exit(327);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtb.b[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(329);
#else
         if ( dtb.b[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(331);
#endif
#ifdef CMPLX
         if ( dtb.d0.b[i][j] != (double)((i+j+2)*2-1)+I*(double)((i+j+2)*2-1) ) exit(333);
#else
         if ( dtb.d0.b[i][j] != createcomplex((double)((i+j+2)*2-1),(double)((i+j+2)*2-1)) ) exit(335);
#endif
      }
   }

/* Test 15 */

   initdtd2(&dtc);

   ret = fnt15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dtc.a[i][j] != (float)(i+j+2)+I*(float)(i+j+2) ) exit(337);
#else
         if ( dtc.a[i][j] != createcomplexf((float)(i+j+2),(float)(i+j+2)) ) exit(339);
#endif
#ifdef CMPLX
         if ( dtc.d1.a[i][j] != (float)((i+j+2)*2-1)+I*(float)((i+j+2)*2-1) ) exit(341);
#else
         if ( dtc.d1.a[i][j] != createcomplexf((float)((i+j+2)*2-1),(float)((i+j+2)*2-1)) ) exit(343);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.a[i][j] != (float)((i+j+2)*2-1)+I*(float)((i+j+2)*2-1) ) exit(345);
#else
         if ( dtc.d1.d0.a[i][j] != createcomplexf((float)((i+j+2)*2-1),(float)((i+j+2)*2-1)) ) exit(347);
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         if ( dtc.b[i][j] != (double)(i+j+2)+I*(double)(i+j+2) ) exit(349);
#else
         if ( dtc.b[i][j] != createcomplex((double)(i+j+2),(double)(i+j+2)) ) exit(351);
#endif
#ifdef CMPLX
         if ( dtc.d1.b[i][j] != (double)((i+j+2)*2-1)+I*(double)((i+j+2)*2-1) ) exit(353);
#else
         if ( dtc.d1.b[i][j] != createcomplex((double)((i+j+2)*2-1),(double)((i+j+2)*2-1)) ) exit(355);
#endif
#ifdef CMPLX
         if ( dtc.d1.d0.b[i][j] != (double)((i+j+2)*2-1)+I*(double)((i+j+2)*2-1) ) exit(357);
#else
         if ( dtc.d1.d0.b[i][j] != createcomplex((double)((i+j+2)*2-1),(double)((i+j+2)*2-1)) ) exit(359);
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
         x->a[i][j] = (float)(i+j+1)+I*(float)(i+j+1);
#else
         x->a[i][j] = createcomplexf((float)(i+j+1),(float)(i+j+1));
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         x->b[i][j] = (double)(i+j+1)+I*(double)(i+j+1);
#else
         x->b[i][j] = createcomplexf((double)(i+j+1),(double)(i+j+1));
#endif
      }
   }

}

void initdtd1(struct dtd1 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         x->a[i][j] = (float)(i+j+1)+I*(float)(i+j+1);
#else
         x->a[i][j] = createcomplexf((float)(i+j+1),(float)(i+j+1));
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         x->b[i][j] = (double)(i+j+1)+I*(double)(i+j+1);
#else
         x->b[i][j] = createcomplexf((double)(i+j+1),(double)(i+j+1));
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
         x->a[i][j] = (float)(i+j+1)+I*(float)(i+j+1);
#else
         x->a[i][j] = createcomplexf((float)(i+j+1),(float)(i+j+1));
#endif
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
#ifdef CMPLX
         x->b[i][j] = (double)(i+j+1)+I*(double)(i+j+1);
#else
         x->b[i][j] = createcomplexf((double)(i+j+1),(double)(i+j+1));
#endif
      }
   }

   initdtd1(&x->d1);

}
