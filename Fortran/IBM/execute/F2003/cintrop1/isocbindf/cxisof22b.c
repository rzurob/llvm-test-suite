
/*
        C code for testcase "fxisof22b.f" and "fxisof23b.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

struct dtd0 {
   int_least32_t a[5][10];
   int_least64_t b[3][6];
};

struct dtd1 {
   int_least32_t a[5][10];
   int_least64_t b[3][6];
   struct dtd0 d0;
};

struct dtd2 {
   int_least32_t a[5][10];
   int_least64_t b[3][6];
   struct dtd1 d1;
};

void sub1(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != i+j+1 ) exit(21);
         dt->a[i][j] = i+j+2;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != i+j+1 ) exit(23);
         dt->b[i][j] = i+j+2;
      }
   }

}

void sub2(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != i+j+1 ) exit(25);
         dt.a[i][j] = i+j+2;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != i+j+1 ) exit(27);
         dt.b[i][j] = i+j+2;
      }
   }

}

void sub3(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != i+j+1 ) exit(29);
         dt->a[i][j] = i+j+2;
         if ( dt->d0.a[i][j] != i+j+1 ) exit(31);
         dt->d0.a[i][j] = i+j+2;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != i+j+1 ) exit(33);
         dt->b[i][j] = i+j+2;
         if ( dt->d0.b[i][j] != i+j+1 ) exit(35);
         dt->d0.b[i][j] = i+j+2;
      }
   }

}

void sub4(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != i+j+1 ) exit(37);
         dt.a[i][j] = i+j+2;
         if ( dt.d0.a[i][j] != i+j+1 ) exit(39);
         dt.d0.a[i][j] = i+j+2;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != i+j+1 ) exit(41);
         dt.b[i][j] = i+j+2;
         if ( dt.d0.b[i][j] != i+j+1 ) exit(43);
         dt.d0.b[i][j] = i+j+2;
      }
   }

}

void sub5(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != i+j+1 ) exit(45);
         dt->a[i][j] = i+j+2;
         if ( dt->d1.a[i][j] != i+j+1 ) exit(47);
         dt->d1.a[i][j] = i+j+2;
         if ( dt->d1.d0.a[i][j] != i+j+1 ) exit(49);
         dt->d1.d0.a[i][j] = i+j+2;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != i+j+1 ) exit(51);
         dt->b[i][j] = i+j+2;
         if ( dt->d1.b[i][j] != i+j+1 ) exit(53);
         dt->d1.b[i][j] = i+j+2;
         if ( dt->d1.d0.b[i][j] != i+j+1 ) exit(55);
         dt->d1.d0.b[i][j] = i+j+2;
      }
   }

}

void sub6(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != i+j+1 ) exit(57);
         dt.a[i][j] = i+j+2;
         if ( dt.d1.a[i][j] != i+j+1 ) exit(59);
         dt.d1.a[i][j] = i+j+2;
         if ( dt.d1.d0.a[i][j] != i+j+1 ) exit(61);
         dt.d1.d0.a[i][j] = i+j+2;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != i+j+1 ) exit(63);
         dt.b[i][j] = i+j+2;
         if ( dt.d1.b[i][j] != i+j+1 ) exit(65);
         dt.d1.b[i][j] = i+j+2;
         if ( dt.d1.d0.b[i][j] != i+j+1 ) exit(67);
         dt.d1.d0.b[i][j] = i+j+2;
      }
   }

}

void sub7(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != i+j+1 ) exit(69);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != i+j+1 ) exit(71);
      }
   }

}

void sub7a(const struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != i+j+1 ) exit(73);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != i+j+1 ) exit(75);
      }
   }

}

void sub8(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != i+j+1 ) exit(77);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != i+j+1 ) exit(79);
      }
   }

}

void sub8a(const struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != i+j+1 ) exit(81);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != i+j+1 ) exit(83);
      }
   }

}

void sub9(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != i+j+1 ) exit(85);
         if ( dt->d0.a[i][j] != i+j+1 ) exit(87);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != i+j+1 ) exit(89);
         if ( dt->d0.b[i][j] != i+j+1 ) exit(91);
      }
   }

}

void sub9a(const struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != i+j+1 ) exit(93);
         if ( dt->d0.a[i][j] != i+j+1 ) exit(95);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != i+j+1 ) exit(97);
         if ( dt->d0.b[i][j] != i+j+1 ) exit(99);
      }
   }

}

void sub10(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != i+j+1 ) exit(101);
         if ( dt.d0.a[i][j] != i+j+1 ) exit(103);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != i+j+1 ) exit(105);
         if ( dt.d0.b[i][j] != i+j+1 ) exit(107);
      }
   }

}

void sub10a(const struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != i+j+1 ) exit(109);
         if ( dt.d0.a[i][j] != i+j+1 ) exit(111);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != i+j+1 ) exit(113);
         if ( dt.d0.b[i][j] != i+j+1 ) exit(115);
      }
   }

}

void sub11(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != i+j+1 ) exit(117);
         if ( dt->d1.a[i][j] != i+j+1 ) exit(119);
         if ( dt->d1.d0.a[i][j] != i+j+1 ) exit(121);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != i+j+1 ) exit(123);
         if ( dt->d1.b[i][j] != i+j+1 ) exit(125);
         if ( dt->d1.d0.b[i][j] != i+j+1 ) exit(127);
      }
   }

}

void sub11a(const struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != i+j+1 ) exit(129);
         if ( dt->d1.a[i][j] != i+j+1 ) exit(131);
         if ( dt->d1.d0.a[i][j] != i+j+1 ) exit(133);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != i+j+1 ) exit(135);
         if ( dt->d1.b[i][j] != i+j+1 ) exit(137);
         if ( dt->d1.d0.b[i][j] != i+j+1 ) exit(141);
      }
   }

}

void sub12(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != i+j+1 ) exit(143);
         if ( dt.d1.a[i][j] != i+j+1 ) exit(145);
         if ( dt.d1.d0.a[i][j] != i+j+1 ) exit(147);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != i+j+1 ) exit(149);
         if ( dt.d1.b[i][j] != i+j+1 ) exit(151);
         if ( dt.d1.d0.b[i][j] != i+j+1 ) exit(153);
      }
   }

}

void sub12a(const struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != i+j+1 ) exit(155);
         if ( dt.d1.a[i][j] != i+j+1 ) exit(157);
         if ( dt.d1.d0.a[i][j] != i+j+1 ) exit(159);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != i+j+1 ) exit(161);
         if ( dt.d1.b[i][j] != i+j+1 ) exit(163);
         if ( dt.d1.d0.b[i][j] != i+j+1 ) exit(165);
      }
   }

}

void sub13(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = i+j+2;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->b[i][j] = i+j+2;
      }
   }

}

void sub14(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = i+j+2;
         dt->d0.a[i][j] = i+j+2;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->b[i][j] = i+j+2;
         dt->d0.b[i][j] = i+j+2;
      }
   }

}

void sub15(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = i+j+2;
         dt->d1.a[i][j] = i+j+2;
         dt->d1.d0.a[i][j] = i+j+2;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->b[i][j] = i+j+2;
         dt->d1.b[i][j] = i+j+2;
         dt->d1.d0.b[i][j] = i+j+2;
      }
   }

}
