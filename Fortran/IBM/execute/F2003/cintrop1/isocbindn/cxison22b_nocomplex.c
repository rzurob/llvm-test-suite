
/*
        C code for testcase "fxison22b.f" and "fxison23b.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

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
         if ( dt->a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(23);
         dt->a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(27);
         dt->b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
      }
   }

}

void sub2(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(31);
         dt.a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(35);
         dt.b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
      }
   }

}

void sub3(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(39);
         dt->a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
         if ( dt->d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(43);
         dt->d0.a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(47);
         dt->b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
         if ( dt->d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(51);
         dt->d0.b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
      }
   }

}

void sub4(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(55);
         dt.a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
         if ( dt.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(59);
         dt.d0.a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(63);
         dt.b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
         if ( dt.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(67);
         dt.d0.b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
      }
   }

}

void sub5(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(71);
         dt->a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
         if ( dt->d1.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(75);
         dt->d1.a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
         if ( dt->d1.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(79);
         dt->d1.d0.a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(83);
         dt->b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
         if ( dt->d1.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(87);
         dt->d1.b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
         if ( dt->d1.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(91);
         dt->d1.d0.b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
      }
   }

}

void sub6(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(95);
         dt.a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
         if ( dt.d1.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(99);
         dt.d1.a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
         if ( dt.d1.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(103);
         dt.d1.d0.a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(107);
         dt.b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
         if ( dt.d1.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(111);
         dt.d1.b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
         if ( dt.d1.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(115);
         dt.d1.d0.b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
      }
   }

}

void sub7(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(119);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(123);
      }
   }

}

void sub7a(const struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(127);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(131);
      }
   }

}

void sub8(struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(135);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(141);
      }
   }

}

void sub8a(const struct dtd0 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(145);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(149);
      }
   }

}

void sub9(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(153);
         if ( dt->d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(157);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(161);
         if ( dt->d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(165);
      }
   }

}

void sub9a(const struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(169);
         if ( dt->d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(173);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(177);
         if ( dt->d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(181);
      }
   }

}

void sub10(struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(185);
         if ( dt.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(189);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(193);
         if ( dt.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(197);
      }
   }

}

void sub10a(const struct dtd1 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(201);
         if ( dt.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(205);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(209);
         if ( dt.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(213);
      }
   }

}

void sub11(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(217);
         if ( dt->d1.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(221);
         if ( dt->d1.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(225);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(229);
         if ( dt->d1.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(233);
         if ( dt->d1.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(237);
      }
   }

}

void sub11a(const struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt->a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(241);
         if ( dt->d1.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(245);
         if ( dt->d1.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(249);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt->b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(255);
         if ( dt->d1.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(259);
         if ( dt->d1.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(263);
      }
   }

}

void sub12(struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(267);
         if ( dt.d1.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(271);
         if ( dt.d1.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(275);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(279);
         if ( dt.d1.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(283);
         if ( dt.d1.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(287);
      }
   }

}

void sub12a(const struct dtd2 dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dt.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(291);
         if ( dt.d1.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(295);
         if ( dt.d1.d0.a[i][j] != createcomplexf((float)(i+j+1),(float)(i+j+1)) ) exit(299);
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         if ( dt.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(303);
         if ( dt.d1.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(307);
         if ( dt.d1.d0.b[i][j] != createcomplex((double)(i+j+1),(double)(i+j+1)) ) exit(311);
      }
   }

}

void sub13(struct dtd0 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
      }
   }

}

void sub14(struct dtd1 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
         dt->d0.a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
         dt->d0.b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
      }
   }

}

void sub15(struct dtd2 *dt) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         dt->a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
         dt->d1.a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
         dt->d1.d0.a[i][j] = createcomplexf((float)(i+j+2),(float)(i+j+2));
      }
   }

   for ( i = 0; i < 3; i++ ) {
      for ( j = 0; j < 6; j++ ) {
         dt->b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
         dt->d1.b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
         dt->d1.d0.b[i][j] = createcomplex((double)(i+j+2),(double)(i+j+2));
      }
   }

}
