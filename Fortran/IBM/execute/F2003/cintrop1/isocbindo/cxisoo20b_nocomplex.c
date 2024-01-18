
/*
        C code for testcase "fxisoo20b.f" and "fxisoo21b.f"
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

long double _Complex fnt1(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(23);
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


   return 0;
}

long double _Complex fnt2(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(27);
         dt.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


   return 0;
}

long double _Complex fnt3(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(31);
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         if ( dt->d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(35);
         dt->d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


   return 0;
}

long double _Complex fnt4(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(39);
         dt.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         if ( dt.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(43);
         dt.d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


   return 0;
}

long double _Complex fnt5(struct dtd2 *dt) {

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


   return 0;
}

long double _Complex fnt6(struct dtd2 dt) {

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


   return 0;
}

long double _Complex fnt7(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(71);
      }
   }


   return 0;
}

long double _Complex fnt7a(const struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(75);
      }
   }


   return 0;
}

long double _Complex fnt8(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(79);
      }
   }


   return 0;
}

long double _Complex fnt8a(const struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(83);
      }
   }


   return 0;
}

long double _Complex fnt9(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(87);
         if ( dt->d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(91);
      }
   }


   return 0;
}

long double _Complex fnt9a(const struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(95);
         if ( dt->d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(99);
      }
   }


   return 0;
}

long double _Complex fnt10(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(103);
         if ( dt.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(107);
      }
   }


   return 0;
}

long double _Complex fnt10a(const struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(111);
         if ( dt.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(115);
      }
   }


   return 0;
}

long double _Complex fnt11(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(119);
         if ( dt->d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(123);
         if ( dt->d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(127);
      }
   }


   return 0;
}

long double _Complex fnt11a(const struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(131);
         if ( dt->d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(135);
         if ( dt->d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(141);
      }
   }


   return 0;
}

long double _Complex fnt12(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(145);
         if ( dt.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(149);
         if ( dt.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(153);
      }
   }


   return 0;
}

long double _Complex fnt12a(const struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(157);
         if ( dt.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(161);
         if ( dt.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(165);
      }
   }


   return 0;
}

long double _Complex fnt13(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


   return 0;
}

long double _Complex fnt14(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         dt->d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


   return 0;
}

long double _Complex fnt15(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         dt->d1.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
         dt->d1.d0.a[i][j] = createcomplexl((long double)(i+j+2),(long double)(i+j+2));
      }
   }


   return 0;
}
