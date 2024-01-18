
/*
        C code for testcase "fxisoq22b.f" and "fxisoq23b.f"
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

void sub1(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(21);
#else
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(23);
#endif
#ifdef CMPLX
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
      }
   }


}

void sub2(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(25);
#else
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(27);
#endif
#ifdef CMPLX
         dt.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
      }
   }


}

void sub3(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(29);
#else
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(31);
#endif
#ifdef CMPLX
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
#ifdef CMPLX
         if ( dt->d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(33);
#else
         if ( dt->d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(35);
#endif
#ifdef CMPLX
         dt->d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt->d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
      }
   }


}

void sub4(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(37);
#else
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(39);
#endif
#ifdef CMPLX
         dt.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
#ifdef CMPLX
         if ( dt.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(41);
#else
         if ( dt.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(43);
#endif
#ifdef CMPLX
         dt.d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt.d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
      }
   }


}

void sub5(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(45);
#else
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(47);
#endif
#ifdef CMPLX
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
#ifdef CMPLX
         if ( dt->d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(49);
#else
         if ( dt->d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(51);
#endif
#ifdef CMPLX
         dt->d1.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt->d1.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
#ifdef CMPLX
         if ( dt->d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(53);
#else
         if ( dt->d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(55);
#endif
#ifdef CMPLX
         dt->d1.d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt->d1.d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
      }
   }


}

void sub6(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(57);
#else
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(59);
#endif
#ifdef CMPLX
         dt.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
#ifdef CMPLX
         if ( dt.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(61);
#else
         if ( dt.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(63);
#endif
#ifdef CMPLX
         dt.d1.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt.d1.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
#ifdef CMPLX
         if ( dt.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(65);
#else
         if ( dt.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(67);
#endif
#ifdef CMPLX
         dt.d1.d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt.d1.d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
      }
   }


}

void sub7(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(69);
#else
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(71);
#endif
      }
   }


}

void sub7a(const struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(73);
#else
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(75);
#endif
      }
   }


}

void sub8(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(77);
#else
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(79);
#endif
      }
   }


}

void sub8a(const struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(81);
#else
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(83);
#endif
      }
   }


}

void sub9(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(85);
#else
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(87);
#endif
#ifdef CMPLX
         if ( dt->d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(89);
#else
         if ( dt->d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(91);
#endif
      }
   }


}

void sub9a(const struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(93);
#else
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(95);
#endif
#ifdef CMPLX
         if ( dt->d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(97);
#else
         if ( dt->d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(99);
#endif
      }
   }


}

void sub10(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(101);
#else
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(103);
#endif
#ifdef CMPLX
         if ( dt.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(105);
#else
         if ( dt.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(107);
#endif
      }
   }


}

void sub10a(const struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(109);
#else
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(111);
#endif
#ifdef CMPLX
         if ( dt.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(113);
#else
         if ( dt.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(115);
#endif
      }
   }


}

void sub11(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(117);
#else
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(119);
#endif
#ifdef CMPLX
         if ( dt->d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(121);
#else
         if ( dt->d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(123);
#endif
#ifdef CMPLX
         if ( dt->d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(125);
#else
         if ( dt->d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(127);
#endif
      }
   }


}

void sub11a(const struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(129);
#else
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(131);
#endif
#ifdef CMPLX
         if ( dt->d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(133);
#else
         if ( dt->d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(135);
#endif
#ifdef CMPLX
         if ( dt->d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(137);
#else
         if ( dt->d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(141);
#endif
      }
   }


}

void sub12(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(143);
#else
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(145);
#endif
#ifdef CMPLX
         if ( dt.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(147);
#else
         if ( dt.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(149);
#endif
#ifdef CMPLX
         if ( dt.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(151);
#else
         if ( dt.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(153);
#endif
      }
   }


}

void sub12a(const struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(155);
#else
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(157);
#endif
#ifdef CMPLX
         if ( dt.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(159);
#else
         if ( dt.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(161);
#endif
#ifdef CMPLX
         if ( dt.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(163);
#else
         if ( dt.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(165);
#endif
      }
   }


}

void sub13(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
      }
   }


}

void sub14(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
#ifdef CMPLX
         dt->d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt->d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
      }
   }


}

void sub15(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
#ifdef CMPLX
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
#ifdef CMPLX
         dt->d1.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt->d1.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
#ifdef CMPLX
         dt->d1.d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
#else
         dt->d1.d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
#endif
      }
   }


}
