
/*
        C code for testcase "fxisoq20b.f" and "fxisoq21b.f"
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

long double _Complex fnt1(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(21);
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


   return 0;
}

long double _Complex fnt2(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(25);
         dt.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


   return 0;
}

long double _Complex fnt3(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(29);
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         if ( dt->d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(33);
         dt->d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


   return 0;
}

long double _Complex fnt4(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(37);
         dt.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         if ( dt.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(41);
         dt.d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


   return 0;
}

long double _Complex fnt5(struct dtd2 *dt) {

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


   return 0;
}

long double _Complex fnt6(struct dtd2 dt) {

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


   return 0;
}

long double _Complex fnt7(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(69);
      }
   }


   return 0;
}

long double _Complex fnt7a(const struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(73);
      }
   }


   return 0;
}

long double _Complex fnt8(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(77);
      }
   }


   return 0;
}

long double _Complex fnt8a(const struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(81);
      }
   }


   return 0;
}

long double _Complex fnt9(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(85);
         if ( dt->d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(89);
      }
   }


   return 0;
}

long double _Complex fnt9a(const struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(93);
         if ( dt->d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(97);
      }
   }


   return 0;
}

long double _Complex fnt10(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(101);
         if ( dt.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(105);
      }
   }


   return 0;
}

long double _Complex fnt10a(const struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(109);
         if ( dt.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(113);
      }
   }


   return 0;
}

long double _Complex fnt11(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(117);
         if ( dt->d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(121);
         if ( dt->d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(125);
      }
   }


   return 0;
}

long double _Complex fnt11a(const struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(129);
         if ( dt->d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(133);
         if ( dt->d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(137);
      }
   }


   return 0;
}

long double _Complex fnt12(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(143);
         if ( dt.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(147);
         if ( dt.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(151);
      }
   }


   return 0;
}

long double _Complex fnt12a(const struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(155);
         if ( dt.d1.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(159);
         if ( dt.d1.d0.a[i][j] != (long double)(i+j+1)+I*(long double)(i+j+1) ) exit(163);
      }
   }


   return 0;
}

long double _Complex fnt13(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


   return 0;
}

long double _Complex fnt14(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         dt->d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


   return 0;
}

long double _Complex fnt15(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         dt->d1.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
         dt->d1.d0.a[i][j] = (long double)(i+j+2)+I*(long double)(i+j+2);
      }
   }


   return 0;
}
