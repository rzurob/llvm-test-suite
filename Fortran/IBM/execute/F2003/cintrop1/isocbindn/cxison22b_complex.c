
/*
        C code for testcase "fxison22b.f" and "fxison23b.f"
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

void sub1(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(21);
         dt->a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(25);
         dt->b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
      }
   }

}

void sub2(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(29);
         dt.a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(33);
         dt.b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
      }
   }

}

void sub3(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(37);
         dt->a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
         if ( dt->d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(41);
         dt->d0.a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(45);
         dt->b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
         if ( dt->d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(49);
         dt->d0.b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
      }
   }

}

void sub4(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(53);
         dt.a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
         if ( dt.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(57);
         dt.d0.a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(61);
         dt.b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
         if ( dt.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(65);
         dt.d0.b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
      }
   }

}

void sub5(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(69);
         dt->a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
         if ( dt->d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(73);
         dt->d1.a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
         if ( dt->d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(77);
         dt->d1.d0.a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(81);
         dt->b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
         if ( dt->d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(85);
         dt->d1.b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
         if ( dt->d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(89);
         dt->d1.d0.b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
      }
   }

}

void sub6(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(93);
         dt.a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
         if ( dt.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(97);
         dt.d1.a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
         if ( dt.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(101);
         dt.d1.d0.a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(105);
         dt.b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
         if ( dt.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(109);
         dt.d1.b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
         if ( dt.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(113);
         dt.d1.d0.b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
      }
   }

}

void sub7(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(117);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(121);
      }
   }

}

void sub7a(const struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(125);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(129);
      }
   }

}

void sub8(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(133);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(137);
      }
   }

}

void sub8a(const struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(143);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(147);
      }
   }

}

void sub9(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(151);
         if ( dt->d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(155);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(159);
         if ( dt->d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(163);
      }
   }

}

void sub9a(const struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(167);
         if ( dt->d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(171);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(175);
         if ( dt->d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(179);
      }
   }

}

void sub10(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(183);
         if ( dt.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(187);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(191);
         if ( dt.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(195);
      }
   }

}

void sub10a(const struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(199);
         if ( dt.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(203);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(207);
         if ( dt.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(211);
      }
   }

}

void sub11(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(215);
         if ( dt->d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(219);
         if ( dt->d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(223);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(227);
         if ( dt->d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(231);
         if ( dt->d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(235);
      }
   }

}

void sub11a(const struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(239);
         if ( dt->d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(243);
         if ( dt->d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(247);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(253);
         if ( dt->d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(257);
         if ( dt->d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(261);
      }
   }

}

void sub12(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(265);
         if ( dt.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(269);
         if ( dt.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(273);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(277);
         if ( dt.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(281);
         if ( dt.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(285);
      }
   }

}

void sub12a(const struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(289);
         if ( dt.d1.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(293);
         if ( dt.d1.d0.a[i][j] != (float)(i+j+1)+I*(float)(i+j+1) ) exit(297);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(301);
         if ( dt.d1.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(305);
         if ( dt.d1.d0.b[i][j] != (double)(i+j+1)+I*(double)(i+j+1) ) exit(309);
      }
   }

}

void sub13(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
      }
   }

}

void sub14(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
         dt->d0.a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
         dt->d0.b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
      }
   }

}

void sub15(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
         dt->d1.a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
         dt->d1.d0.a[i][j] = (float)(i+j+2)+I*(float)(i+j+2);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
         dt->d1.b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
         dt->d1.d0.b[i][j] = (double)(i+j+2)+I*(double)(i+j+2);
      }
   }

}