
/*
        C code for testcase "fxisoq22b.f" and "fxisoq23b.f"
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

void sub1(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(21);
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


}

void sub2(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(25);
         dt.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


}

void sub3(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(29);
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         if ( dt->d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(33);
         dt->d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


}

void sub4(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(37);
         dt.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         if ( dt.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(41);
         dt.d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


}

void sub5(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(45);
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         if ( dt->d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(49);
         dt->d1.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         if ( dt->d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(53);
         dt->d1.d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


}

void sub6(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(57);
         dt.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         if ( dt.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(61);
         dt.d1.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         if ( dt.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(65);
         dt.d1.d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


}

void sub7(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(69);
      }
   }


}

void sub7a(const struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(73);
      }
   }


}

void sub8(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(77);
      }
   }


}

void sub8a(const struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(81);
      }
   }


}

void sub9(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(85);
         if ( dt->d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(89);
      }
   }


}

void sub9a(const struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(93);
         if ( dt->d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(97);
      }
   }


}

void sub10(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(101);
         if ( dt.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(105);
      }
   }


}

void sub10a(const struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(109);
         if ( dt.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(113);
      }
   }


}

void sub11(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(117);
         if ( dt->d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(121);
         if ( dt->d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(125);
      }
   }


}

void sub11a(const struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(129);
         if ( dt->d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(133);
         if ( dt->d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(137);
      }
   }


}

void sub12(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(143);
         if ( dt.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(147);
         if ( dt.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(151);
      }
   }


}

void sub12a(const struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(155);
         if ( dt.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(159);
         if ( dt.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(163);
      }
   }


}

void sub13(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


}

void sub14(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         dt->d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


}

void sub15(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         dt->d1.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         dt->d1.d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


}
