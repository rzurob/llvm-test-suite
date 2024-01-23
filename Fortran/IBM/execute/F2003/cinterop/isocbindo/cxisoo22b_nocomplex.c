
/*
        C code for testcase "fxisoo22b.f" and "fxisoo23b.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

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
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(23);
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


}

void sub2(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(27);
         dt.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


}

void sub3(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(31);
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         if ( dt->d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(35);
         dt->d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


}

void sub4(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(39);
         dt.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         if ( dt.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(43);
         dt.d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


}

void sub5(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(47);
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         if ( dt->d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(51);
         dt->d1.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         if ( dt->d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(55);
         dt->d1.d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


}

void sub6(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(59);
         dt.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         if ( dt.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(63);
         dt.d1.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         if ( dt.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(67);
         dt.d1.d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


}

void sub7(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(71);
      }
   }


}

void sub7a(const struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(75);
      }
   }


}

void sub8(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(79);
      }
   }


}

void sub8a(const struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(83);
      }
   }


}

void sub9(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(87);
         if ( dt->d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(91);
      }
   }


}

void sub9a(const struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(95);
         if ( dt->d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(99);
      }
   }


}

void sub10(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(103);
         if ( dt.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(107);
      }
   }


}

void sub10a(const struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(111);
         if ( dt.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(115);
      }
   }


}

void sub11(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(119);
         if ( dt->d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(123);
         if ( dt->d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(127);
      }
   }


}

void sub11a(const struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(131);
         if ( dt->d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(135);
         if ( dt->d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(141);
      }
   }


}

void sub12(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(145);
         if ( dt.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(149);
         if ( dt.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(153);
      }
   }


}

void sub12a(const struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(157);
         if ( dt.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(161);
         if ( dt.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(165);
      }
   }


}

void sub13(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


}

void sub14(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         dt->d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


}

void sub15(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         dt->d1.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         dt->d1.d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


}
