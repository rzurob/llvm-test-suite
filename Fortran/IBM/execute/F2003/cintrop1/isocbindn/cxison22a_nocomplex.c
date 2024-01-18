/*
        C code for testcase "fxison22a.f" and "fxison23a.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

struct dts0 {
   float _Complex a[5];
   double _Complex b[3];
};

struct dts1 {
   float _Complex a[5];
   double _Complex b[3];
   struct dts0 d0;
};

struct dts2 {
   float _Complex a[5];
   double _Complex b[3];
   struct dts1 d1;
};

void sub1(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(23);
      dt->a[i] = createcomplexf((float)(i+2),(float)(i+2));
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(27);
      dt->b[i] = createcomplex((double)(i+2),(double)(i+2));
   }

}

void sub2(struct dts0 dt) {
   int i; 


   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(31);
      dt.a[i] = createcomplexf((float)(i+2),(float)(i+2));
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(35);
      dt.b[i] = createcomplex((double)(i+2),(double)(i+2));
   }

}

void sub3(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(39);
      dt->a[i] = dt->a[i] + createcomplexf((float)(i+1),(float)(i+1));
      if ( dt->d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(43);
      dt->d0.a[i] = dt->d0.a[i] + createcomplexf((float)(i+1),(float)(i+1));
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(47);
      dt->b[i] = dt->b[i] + createcomplex((double)(i+1),(double)(i+1));
      if ( dt->d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(51);
      dt->d0.b[i] = dt->d0.b[i] + createcomplex((double)(i+1),(double)(i+1));
   }

}

void sub4(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(55);
      dt.a[i] = dt.a[i] + createcomplexf((float)(i+1),(float)(i+1));
      if ( dt.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(59);
      dt.d0.a[i] = dt.d0.a[i] + createcomplexf((float)(i+1),(float)(i+1));
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(63);
      dt.b[i] = dt.b[i] + createcomplex((double)(i+1),(double)(i+1));
      if ( dt.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(67);
      dt.d0.b[i] = dt.d0.b[i] + createcomplex((double)(i+1),(double)(i+1));
   }

}

void sub5(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(71);
      dt->a[i] = dt->a[i] + createcomplexf((float)(i+1),(float)(i+1));
      if ( dt->d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(75);
      dt->d1.a[i] = dt->d1.a[i] + createcomplexf((float)(i+1),(float)(i+1));
      if ( dt->d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(79);
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + createcomplexf((float)(i+1),(float)(i+1));
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(83);
      dt->b[i] = dt->b[i] + createcomplex((double)(i+1),(double)(i+1));
      if ( dt->d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(87);
      dt->d1.b[i] = dt->d1.b[i] + createcomplex((double)(i+1),(double)(i+1));
      if ( dt->d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(91);
      dt->d1.d0.b[i] = dt->d1.d0.b[i] + createcomplex((double)(i+1),(double)(i+1));
   }

}

void sub6(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(95);
      dt.a[i] = dt.a[i] + createcomplexf((float)(i+1),(float)(i+1));
      if ( dt.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(99);
      dt.d1.a[i] = dt.d1.a[i] + createcomplexf((float)(i+1),(float)(i+1));
      if ( dt.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(103);
      dt.d1.d0.a[i] = dt.d1.d0.a[i] + createcomplexf((float)(i+1),(float)(i+1));
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(107);
      dt.b[i] = dt.b[i] + createcomplex((double)(i+1),(double)(i+1));
      if ( dt.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(111);
      dt.d1.b[i] = dt.d1.b[i] + createcomplex((double)(i+1),(double)(i+1));
      if ( dt.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(115);
      dt.d1.d0.b[i] = dt.d1.d0.b[i] + createcomplex((double)(i+1),(double)(i+1));
   }

}

void sub7(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(119);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(123);
   }

}

void sub7a(const struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(127);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(131);
   }

}

void sub8(struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(135);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(141);
   }

}

void sub8a(const struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(145);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(149);
   }

}

void sub9(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(153);
      if ( dt->d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(157);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(161);
      if ( dt->d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(165);
   }

}

void sub9a(const struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(169);
      if ( dt->d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(173);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(177);
      if ( dt->d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(181);
   }

}

void sub10(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(185);
      if ( dt.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(189);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(193);
      if ( dt.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(197);
   }

}

void sub10a(const struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(201);
      if ( dt.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(205);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(209);
      if ( dt.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(213);
   }

}

void sub11(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(217);
      if ( dt->d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(221);
      if ( dt->d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(225);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(229);
      if ( dt->d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(233);
      if ( dt->d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(237);
   }

}

void sub11a(const struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(241);
      if ( dt->d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(245);
      if ( dt->d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(249);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(255);
      if ( dt->d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(259);
      if ( dt->d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(263);
   }

}

void sub12(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(267);
      if ( dt.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(271);
      if ( dt.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(275);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(279);
      if ( dt.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(283);
      if ( dt.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(287);
   }

}

void sub12a(const struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(291);
      if ( dt.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(295);
      if ( dt.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(299);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(303);
      if ( dt.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(307);
      if ( dt.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(311);
   }

}

void sub13(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + createcomplexf((float)(i+1),(float)(i+1));
   }

   for ( i = 0; i < 3; i++ ) {
      dt->b[i] = dt->b[i] + createcomplex((double)(i+1),(double)(i+1));
   }

}

void sub14(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + createcomplexf((float)(i+1),(float)(i+1));
      dt->d0.a[i] = dt->d0.a[i] + createcomplexf((float)(i+1),(float)(i+1));
   }

   for ( i = 0; i < 3; i++ ) {
      dt->b[i] = dt->b[i] + createcomplex((double)(i+1),(double)(i+1));
      dt->d0.b[i] = dt->d0.b[i] + createcomplex((double)(i+1),(double)(i+1));
   }

}

void sub15(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + createcomplexf((float)(i+1),(float)(i+1));
      dt->d1.a[i] = dt->d1.a[i] + createcomplexf((float)(i+1),(float)(i+1));
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + createcomplexf((float)(i+1),(float)(i+1));
   }

   for ( i = 0; i < 3; i++ ) {
      dt->b[i] = dt->b[i] + createcomplex((double)(i+1),(double)(i+1));
      dt->d1.b[i] = dt->d1.b[i] + createcomplex((double)(i+1),(double)(i+1));
      dt->d1.d0.b[i] = dt->d1.d0.b[i] + createcomplex((double)(i+1),(double)(i+1));
   }

}
