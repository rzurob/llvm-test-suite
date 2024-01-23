
/*
        C code for testcase "fxisok20b.f" and "fxisok21b.f"
*/

#include <stdio.h>
#include <stdlib.h>

struct dtd0 {
   char a[4][6];
   signed char b[3][6];
};

struct dtd1 {
   char a[4][6];
   signed char b[3][6];
   struct dtd0 d0;
};

struct dtd2 {
   char a[4][6];
   signed char b[3][6];
   struct dtd1 d1;
};

char fnt1(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->a[i][j] != 'A'+i*6+j ) exit(21);
         dt->a[i][j] = 'A'+i*6+j+1;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != 'A'+i*6+j ) exit(23);
         dt->b[i][j] = 'A'+i*6+j+1;
      }
   }

   return 0;
}

char fnt2(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.a[i][j] != 'A'+i*6+j ) exit(25);
         dt.a[i][j] = 'A'+i*6+j+1;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != 'A'+i*6+j ) exit(27);
         dt.b[i][j] = 'A'+i*6+j+1;
      }
   }

   return 0;
}

char fnt3(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->a[i][j] != 'A'+i*6+j ) exit(29);
         dt->a[i][j] = 'A'+i*6+j+1;
         if ( dt->d0.a[i][j] != 'A'+i*6+j ) exit(31);
         dt->d0.a[i][j] = 'A'+i*6+j+1;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != 'A'+i*6+j ) exit(33);
         dt->b[i][j] = 'A'+i*6+j+1;
         if ( dt->d0.b[i][j] != 'A'+i*6+j ) exit(35);
         dt->d0.b[i][j] = 'A'+i*6+j+1;
      }
   }

   return 0;
}

char fnt4(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.a[i][j] != 'A'+i*6+j ) exit(37);
         dt.a[i][j] = 'A'+i*6+j+1;
         if ( dt.d0.a[i][j] != 'A'+i*6+j ) exit(39);
         dt.d0.a[i][j] = 'A'+i*6+j+1;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != 'A'+i*6+j ) exit(41);
         dt.b[i][j] = 'A'+i*6+j+1;
         if ( dt.d0.b[i][j] != 'A'+i*6+j ) exit(43);
         dt.d0.b[i][j] = 'A'+i*6+j+1;
      }
   }

   return 0;
}

char fnt5(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->a[i][j] != 'A'+i*6+j ) exit(45);
         dt->a[i][j] = 'A'+i*6+j+1;
         if ( dt->d1.a[i][j] != 'A'+i*6+j ) exit(47);
         dt->d1.a[i][j] = 'A'+i*6+j+1;
         if ( dt->d1.d0.a[i][j] != 'A'+i*6+j ) exit(49);
         dt->d1.d0.a[i][j] = 'A'+i*6+j+1;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != 'A'+i*6+j ) exit(51);
         dt->b[i][j] = 'A'+i*6+j+1;
         if ( dt->d1.b[i][j] != 'A'+i*6+j ) exit(53);
         dt->d1.b[i][j] = 'A'+i*6+j+1;
         if ( dt->d1.d0.b[i][j] != 'A'+i*6+j ) exit(55);
         dt->d1.d0.b[i][j] = 'A'+i*6+j+1;
      }
   }

   return 0;
}

char fnt6(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.a[i][j] != 'A'+i*6+j ) exit(57);
         dt.a[i][j] = 'A'+i*6+j+1;
         if ( dt.d1.a[i][j] != 'A'+i*6+j ) exit(59);
         dt.d1.a[i][j] = 'A'+i*6+j+1;
         if ( dt.d1.d0.a[i][j] != 'A'+i*6+j ) exit(61);
         dt.d1.d0.a[i][j] = 'A'+i*6+j+1;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != 'A'+i*6+j ) exit(63);
         dt.b[i][j] = 'A'+i*6+j+1;
         if ( dt.d1.b[i][j] != 'A'+i*6+j ) exit(65);
         dt.d1.b[i][j] = 'A'+i*6+j+1;
         if ( dt.d1.d0.b[i][j] != 'A'+i*6+j ) exit(67);
         dt.d1.d0.b[i][j] = 'A'+i*6+j+1;
      }
   }

   return 0;
}

char fnt7(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->a[i][j] != 'A'+i*6+j ) exit(69);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != 'A'+i*6+j ) exit(71);
      }
   }

   return 0;
}

char fnt7a(const struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->a[i][j] != 'A'+i*6+j ) exit(73);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != 'A'+i*6+j ) exit(75);
      }
   }

   return 0;
}

char fnt8(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.a[i][j] != 'A'+i*6+j ) exit(77);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != 'A'+i*6+j ) exit(79);
      }
   }

   return 0;
}

char fnt8a(const struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.a[i][j] != 'A'+i*6+j ) exit(81);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != 'A'+i*6+j ) exit(83);
      }
   }

   return 0;
}

char fnt9(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->a[i][j] != 'A'+i*6+j ) exit(85);
         if ( dt->d0.a[i][j] != 'A'+i*6+j ) exit(87);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != 'A'+i*6+j ) exit(89);
         if ( dt->d0.b[i][j] != 'A'+i*6+j ) exit(91);
      }
   }

   return 0;
}

char fnt9a(const struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->a[i][j] != 'A'+i*6+j ) exit(93);
         if ( dt->d0.a[i][j] != 'A'+i*6+j ) exit(95);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != 'A'+i*6+j ) exit(97);
         if ( dt->d0.b[i][j] != 'A'+i*6+j ) exit(99);
      }
   }

   return 0;
}

char fnt10(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.a[i][j] != 'A'+i*6+j ) exit(101);
         if ( dt.d0.a[i][j] != 'A'+i*6+j ) exit(103);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != 'A'+i*6+j ) exit(105);
         if ( dt.d0.b[i][j] != 'A'+i*6+j ) exit(107);
      }
   }

   return 0;
}

char fnt10a(const struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.a[i][j] != 'A'+i*6+j ) exit(109);
         if ( dt.d0.a[i][j] != 'A'+i*6+j ) exit(111);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != 'A'+i*6+j ) exit(113);
         if ( dt.d0.b[i][j] != 'A'+i*6+j ) exit(115);
      }
   }

   return 0;
}

char fnt11(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->a[i][j] != 'A'+i*6+j ) exit(117);
         if ( dt->d1.a[i][j] != 'A'+i*6+j ) exit(119);
         if ( dt->d1.d0.a[i][j] != 'A'+i*6+j ) exit(121);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != 'A'+i*6+j ) exit(123);
         if ( dt->d1.b[i][j] != 'A'+i*6+j ) exit(125);
         if ( dt->d1.d0.b[i][j] != 'A'+i*6+j ) exit(127);
      }
   }

   return 0;
}

char fnt11a(const struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->a[i][j] != 'A'+i*6+j ) exit(129);
         if ( dt->d1.a[i][j] != 'A'+i*6+j ) exit(131);
         if ( dt->d1.d0.a[i][j] != 'A'+i*6+j ) exit(133);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != 'A'+i*6+j ) exit(135);
         if ( dt->d1.b[i][j] != 'A'+i*6+j ) exit(137);
         if ( dt->d1.d0.b[i][j] != 'A'+i*6+j ) exit(141);
      }
   }

   return 0;
}

char fnt12(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.a[i][j] != 'A'+i*6+j ) exit(143);
         if ( dt.d1.a[i][j] != 'A'+i*6+j ) exit(145);
         if ( dt.d1.d0.a[i][j] != 'A'+i*6+j ) exit(147);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != 'A'+i*6+j ) exit(149);
         if ( dt.d1.b[i][j] != 'A'+i*6+j ) exit(151);
         if ( dt.d1.d0.b[i][j] != 'A'+i*6+j ) exit(153);
      }
   }

   return 0;
}

char fnt12a(const struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.a[i][j] != 'A'+i*6+j ) exit(155);
         if ( dt.d1.a[i][j] != 'A'+i*6+j ) exit(157);
         if ( dt.d1.d0.a[i][j] != 'A'+i*6+j ) exit(159);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != 'A'+i*6+j ) exit(161);
         if ( dt.d1.b[i][j] != 'A'+i*6+j ) exit(163);
         if ( dt.d1.d0.b[i][j] != 'A'+i*6+j ) exit(165);
      }
   }

   return 0;
}

char fnt13(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->a[i][j] = 'A'+i*6+j+1;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->b[i][j] = 'A'+i*6+j+1;
      }
   }

   return 0;
}

char fnt14(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->a[i][j] = 'A'+i*6+j+1;
         dt->d0.a[i][j] = 'A'+i*6+j+1;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->b[i][j] = 'A'+i*6+j+1;
         dt->d0.b[i][j] = 'A'+i*6+j+1;
      }
   }

   return 0;
}

char fnt15(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 4; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->a[i][j] = 'A'+i*6+j+1;
         dt->d1.a[i][j] = 'A'+i*6+j+1;
         dt->d1.d0.a[i][j] = 'A'+i*6+j+1;
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->b[i][j] = 'A'+i*6+j+1;
         dt->d1.b[i][j] = 'A'+i*6+j+1;
         dt->d1.d0.b[i][j] = 'A'+i*6+j+1;
      }
   }

   return 0;
}
