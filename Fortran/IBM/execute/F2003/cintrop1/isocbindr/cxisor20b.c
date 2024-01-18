
/*
        C code for testcase "fxisor20b.f" and "fxisor21b.f"
*/

#include <stdio.h>
#include <stdlib.h>


struct dtd0 {
   _Bool a[5][10];
};

struct dtd1 {
   _Bool a[5][10];
   struct dtd0 d0;
};

struct dtd2 {
   _Bool a[5][10];
   struct dtd1 d1;
};

_Bool fnt1(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != 1 ) exit(21);
         dt->a[i][j] = 0;
      }
   }


   return 0;
}

_Bool fnt2(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != 1 ) exit(23);
         dt.a[i][j] = 0;
      }
   }


   return 0;
}

_Bool fnt3(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != 1 ) exit(25);
         dt->a[i][j] = 0;
         if ( dt->d0.a[i][j] != 1 ) exit(27);
         dt->d0.a[i][j] = 0;
      }
   }


   return 0;
}

_Bool fnt4(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != 1 ) exit(29);
         dt.a[i][j] = 0;
         if ( dt.d0.a[i][j] != 1 ) exit(31);
         dt.d0.a[i][j] = 0;
      }
   }


   return 0;
}

_Bool fnt5(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != 1 ) exit(33);
         dt->a[i][j] = 0;
         if ( dt->d1.a[i][j] != 1 ) exit(35);
         dt->d1.a[i][j] = 0;
         if ( dt->d1.d0.a[i][j] != 1 ) exit(37);
         dt->d1.d0.a[i][j] = 0;
      }
   }


   return 0;
}

_Bool fnt6(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != 1 ) exit(39);
         dt.a[i][j] = 0;
         if ( dt.d1.a[i][j] != 1 ) exit(41);
         dt.d1.a[i][j] = 0;
         if ( dt.d1.d0.a[i][j] != 1 ) exit(43);
         dt.d1.d0.a[i][j] = 0;
      }
   }


   return 0;
}

_Bool fnt7(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != 1 ) exit(45);
      }
   }


   return 0;
}

_Bool fnt7a(const struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != 1 ) exit(47);
      }
   }


   return 0;
}

_Bool fnt8(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != 1 ) exit(49);
      }
   }


   return 0;
}

_Bool fnt8a(const struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != 1 ) exit(51);
      }
   }


   return 0;
}

_Bool fnt9(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != 1 ) exit(53);
         if ( dt->d0.a[i][j] != 1 ) exit(55);
      }
   }


   return 0;
}

_Bool fnt9a(const struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != 1 ) exit(57);
         if ( dt->d0.a[i][j] != 1 ) exit(59);
      }
   }


   return 0;
}

_Bool fnt10(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != 1 ) exit(61);
         if ( dt.d0.a[i][j] != 1 ) exit(63);
      }
   }


   return 0;
}

_Bool fnt10a(const struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != 1 ) exit(65);
         if ( dt.d0.a[i][j] != 1 ) exit(67);
      }
   }


   return 0;
}

_Bool fnt11(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != 1 ) exit(69);
         if ( dt->d1.a[i][j] != 1 ) exit(71);
         if ( dt->d1.d0.a[i][j] != 1 ) exit(73);
      }
   }


   return 0;
}

_Bool fnt11a(const struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != 1 ) exit(75);
         if ( dt->d1.a[i][j] != 1 ) exit(77);
         if ( dt->d1.d0.a[i][j] != 1 ) exit(79);
      }
   }


   return 0;
}

_Bool fnt12(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != 1 ) exit(81);
         if ( dt.d1.a[i][j] != 1 ) exit(83);
         if ( dt.d1.d0.a[i][j] != 1 ) exit(85);
      }
   }


   return 0;
}

_Bool fnt12a(const struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != 1 ) exit(87);
         if ( dt.d1.a[i][j] != 1 ) exit(89);
         if ( dt.d1.d0.a[i][j] != 1 ) exit(91);
      }
   }


   return 0;
}

_Bool fnt13(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = 0;
      }
   }


   return 0;
}

_Bool fnt14(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = 0;
         dt->d0.a[i][j] = 0;
      }
   }


   return 0;
}

_Bool fnt15(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = 0;
         dt->d1.a[i][j] = 0;
         dt->d1.d0.a[i][j] = 0;
      }
   }


   return 0;
}
