/*
        C code for testcase "fxison20a.f" and "fxison21a.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

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

int fnt1(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(21);
#else
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(23);
#endif
#ifdef CMPLX
      dt->a[i] = (float)(i+2)+I*(float)(i+2);
#else
      dt->a[i] = createcomplexf((float)(i+2),(float)(i+2));
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(25);
#else
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(27);
#endif
#ifdef CMPLX
      dt->b[i] = (double)(i+2)+I*(double)(i+2);
#else
      dt->b[i] = createcomplex((double)(i+2),(double)(i+2));
#endif
   }

   return 0;
}

int fnt2(struct dts0 dt) {
   int i; 


   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(29);
#else
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(31);
#endif
#ifdef CMPLX
      dt.a[i] = (float)(i+2)+I*(float)(i+2);
#else
      dt.a[i] = createcomplexf((float)(i+2),(float)(i+2));
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(33);
#else
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(35);
#endif
#ifdef CMPLX
      dt.b[i] = (double)(i+2)+I*(double)(i+2);
#else
      dt.b[i] = createcomplex((double)(i+2),(double)(i+2));
#endif
   }

   return 0;
}

int fnt3(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(37);
#else
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(39);
#endif
#ifdef CMPLX
      dt->a[i] = dt->a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt->a[i] = dt->a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
#ifdef CMPLX
      if ( dt->d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(41);
#else
      if ( dt->d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(43);
#endif
#ifdef CMPLX
      dt->d0.a[i] = dt->d0.a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt->d0.a[i] = dt->d0.a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(45);
#else
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(47);
#endif
#ifdef CMPLX
      dt->b[i] = dt->b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt->b[i] = dt->b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
#ifdef CMPLX
      if ( dt->d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(49);
#else
      if ( dt->d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(51);
#endif
#ifdef CMPLX
      dt->d0.b[i] = dt->d0.b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt->d0.b[i] = dt->d0.b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
   }

   return 0;
}

int fnt4(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(53);
#else
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(55);
#endif
#ifdef CMPLX
      dt.a[i] = dt.a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt.a[i] = dt.a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
#ifdef CMPLX
      if ( dt.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(57);
#else
      if ( dt.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(59);
#endif
#ifdef CMPLX
      dt.d0.a[i] = dt.d0.a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt.d0.a[i] = dt.d0.a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(61);
#else
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(63);
#endif
#ifdef CMPLX
      dt.b[i] = dt.b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt.b[i] = dt.b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
#ifdef CMPLX
      if ( dt.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(65);
#else
      if ( dt.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(67);
#endif
#ifdef CMPLX
      dt.d0.b[i] = dt.d0.b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt.d0.b[i] = dt.d0.b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
   }

   return 0;
}

int fnt5(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(69);
#else
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(71);
#endif
#ifdef CMPLX
      dt->a[i] = dt->a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt->a[i] = dt->a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
#ifdef CMPLX
      if ( dt->d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(73);
#else
      if ( dt->d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(75);
#endif
#ifdef CMPLX
      dt->d1.a[i] = dt->d1.a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt->d1.a[i] = dt->d1.a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
#ifdef CMPLX
      if ( dt->d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(77);
#else
      if ( dt->d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(79);
#endif
#ifdef CMPLX
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(81);
#else
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(83);
#endif
#ifdef CMPLX
      dt->b[i] = dt->b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt->b[i] = dt->b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
#ifdef CMPLX
      if ( dt->d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(85);
#else
      if ( dt->d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(87);
#endif
#ifdef CMPLX
      dt->d1.b[i] = dt->d1.b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt->d1.b[i] = dt->d1.b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
#ifdef CMPLX
      if ( dt->d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(89);
#else
      if ( dt->d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(91);
#endif
#ifdef CMPLX
      dt->d1.d0.b[i] = dt->d1.d0.b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt->d1.d0.b[i] = dt->d1.d0.b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
   }

   return 0;
}

int fnt6(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(93);
#else
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(95);
#endif
#ifdef CMPLX
      dt.a[i] = dt.a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt.a[i] = dt.a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
#ifdef CMPLX
      if ( dt.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(97);
#else
      if ( dt.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(99);
#endif
#ifdef CMPLX
      dt.d1.a[i] = dt.d1.a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt.d1.a[i] = dt.d1.a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
#ifdef CMPLX
      if ( dt.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(101);
#else
      if ( dt.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(103);
#endif
#ifdef CMPLX
      dt.d1.d0.a[i] = dt.d1.d0.a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt.d1.d0.a[i] = dt.d1.d0.a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(105);
#else
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(107);
#endif
#ifdef CMPLX
      dt.b[i] = dt.b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt.b[i] = dt.b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
#ifdef CMPLX
      if ( dt.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(109);
#else
      if ( dt.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(111);
#endif
#ifdef CMPLX
      dt.d1.b[i] = dt.d1.b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt.d1.b[i] = dt.d1.b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
#ifdef CMPLX
      if ( dt.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(113);
#else
      if ( dt.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(115);
#endif
#ifdef CMPLX
      dt.d1.d0.b[i] = dt.d1.d0.b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt.d1.d0.b[i] = dt.d1.d0.b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
   }

   return 0;
}

int fnt7(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(117);
#else
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(119);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(121);
#else
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(123);
#endif
   }

   return 0;
}

int fnt7a(const struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(125);
#else
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(127);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(129);
#else
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(131);
#endif
   }

   return 0;
}

int fnt8(struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(133);
#else
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(135);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(137);
#else
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(141);
#endif
   }

   return 0;
}

int fnt8a(const struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(143);
#else
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(145);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(147);
#else
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(149);
#endif
   }

   return 0;
}

int fnt9(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(151);
#else
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(153);
#endif
#ifdef CMPLX
      if ( dt->d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(155);
#else
      if ( dt->d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(157);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(159);
#else
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(161);
#endif
#ifdef CMPLX
      if ( dt->d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(163);
#else
      if ( dt->d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(165);
#endif
   }

   return 0;
}

int fnt9a(const struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(167);
#else
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(169);
#endif
#ifdef CMPLX
      if ( dt->d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(171);
#else
      if ( dt->d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(173);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(175);
#else
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(177);
#endif
#ifdef CMPLX
      if ( dt->d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(179);
#else
      if ( dt->d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(181);
#endif
   }

   return 0;
}

int fnt10(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(183);
#else
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(185);
#endif
#ifdef CMPLX
      if ( dt.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(187);
#else
      if ( dt.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(189);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(191);
#else
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(193);
#endif
#ifdef CMPLX
      if ( dt.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(195);
#else
      if ( dt.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(197);
#endif
   }

   return 0;
}

int fnt10a(const struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(199);
#else
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(201);
#endif
#ifdef CMPLX
      if ( dt.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(203);
#else
      if ( dt.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(205);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(207);
#else
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(209);
#endif
#ifdef CMPLX
      if ( dt.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(211);
#else
      if ( dt.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(213);
#endif
   }

   return 0;
}

int fnt11(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(215);
#else
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(217);
#endif
#ifdef CMPLX
      if ( dt->d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(219);
#else
      if ( dt->d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(221);
#endif
#ifdef CMPLX
      if ( dt->d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(223);
#else
      if ( dt->d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(225);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(227);
#else
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(229);
#endif
#ifdef CMPLX
      if ( dt->d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(231);
#else
      if ( dt->d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(233);
#endif
#ifdef CMPLX
      if ( dt->d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(235);
#else
      if ( dt->d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(237);
#endif
   }

   return 0;
}

int fnt11a(const struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(239);
#else
      if ( dt->a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(241);
#endif
#ifdef CMPLX
      if ( dt->d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(243);
#else
      if ( dt->d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(245);
#endif
#ifdef CMPLX
      if ( dt->d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(247);
#else
      if ( dt->d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(249);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(253);
#else
      if ( dt->b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(255);
#endif
#ifdef CMPLX
      if ( dt->d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(257);
#else
      if ( dt->d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(259);
#endif
#ifdef CMPLX
      if ( dt->d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(261);
#else
      if ( dt->d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(263);
#endif
   }

   return 0;
}

int fnt12(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(265);
#else
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(267);
#endif
#ifdef CMPLX
      if ( dt.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(269);
#else
      if ( dt.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(271);
#endif
#ifdef CMPLX
      if ( dt.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(273);
#else
      if ( dt.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(275);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(277);
#else
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(279);
#endif
#ifdef CMPLX
      if ( dt.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(281);
#else
      if ( dt.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(283);
#endif
#ifdef CMPLX
      if ( dt.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(285);
#else
      if ( dt.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(287);
#endif
   }

   return 0;
}

int fnt12a(const struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(289);
#else
      if ( dt.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(291);
#endif
#ifdef CMPLX
      if ( dt.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(293);
#else
      if ( dt.d1.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(295);
#endif
#ifdef CMPLX
      if ( dt.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(297);
#else
      if ( dt.d1.d0.a[i] != createcomplexf((float)(i+1),(float)(i+1)) ) exit(299);
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(301);
#else
      if ( dt.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(303);
#endif
#ifdef CMPLX
      if ( dt.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(305);
#else
      if ( dt.d1.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(307);
#endif
#ifdef CMPLX
      if ( dt.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(309);
#else
      if ( dt.d1.d0.b[i] != createcomplex((double)(i+1),(double)(i+1)) ) exit(311);
#endif
   }

   return 0;
}

int fnt13(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      dt->a[i] = dt->a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt->a[i] = dt->a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      dt->b[i] = dt->b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt->b[i] = dt->b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
   }

   return 0;
}

int fnt14(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      dt->a[i] = dt->a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt->a[i] = dt->a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
#ifdef CMPLX
      dt->d0.a[i] = dt->d0.a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt->d0.a[i] = dt->d0.a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      dt->b[i] = dt->b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt->b[i] = dt->b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
#ifdef CMPLX
      dt->d0.b[i] = dt->d0.b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt->d0.b[i] = dt->d0.b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
   }

   return 0;
}

int fnt15(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
#ifdef CMPLX
      dt->a[i] = dt->a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt->a[i] = dt->a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
#ifdef CMPLX
      dt->d1.a[i] = dt->d1.a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt->d1.a[i] = dt->d1.a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
#ifdef CMPLX
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + (float)(i+1)+I*(float)(i+1);
#else
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + createcomplexf((float)(i+1),(float)(i+1));
#endif
   }

   for ( i = 0; i < 3; i++ ) {
#ifdef CMPLX
      dt->b[i] = dt->b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt->b[i] = dt->b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
#ifdef CMPLX
      dt->d1.b[i] = dt->d1.b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt->d1.b[i] = dt->d1.b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
#ifdef CMPLX
      dt->d1.d0.b[i] = dt->d1.d0.b[i] + (double)(i+1)+I*(double)(i+1);
#else
      dt->d1.d0.b[i] = dt->d1.d0.b[i] + createcomplex((double)(i+1),(double)(i+1));
#endif
   }

   return 0;
}
