/*
        C code for testcase "fxison20a.f" and "fxison21a.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

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
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(21);
      dt->a[i] = (float)(i+2)+I*(float)(i+2);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(25);
      dt->b[i] = (double)(i+2)+I*(double)(i+2);
   }

   return 0;
}

int fnt2(struct dts0 dt) {
   int i; 


   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(29);
      dt.a[i] = (float)(i+2)+I*(float)(i+2);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(33);
      dt.b[i] = (double)(i+2)+I*(double)(i+2);
   }

   return 0;
}

int fnt3(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(37);
      dt->a[i] = dt->a[i] + (float)(i+1)+I*(float)(i+1);
      if ( dt->d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(41);
      dt->d0.a[i] = dt->d0.a[i] + (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(45);
      dt->b[i] = dt->b[i] + (double)(i+1)+I*(double)(i+1);
      if ( dt->d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(49);
      dt->d0.b[i] = dt->d0.b[i] + (double)(i+1)+I*(double)(i+1);
   }

   return 0;
}

int fnt4(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(53);
      dt.a[i] = dt.a[i] + (float)(i+1)+I*(float)(i+1);
      if ( dt.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(57);
      dt.d0.a[i] = dt.d0.a[i] + (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(61);
      dt.b[i] = dt.b[i] + (double)(i+1)+I*(double)(i+1);
      if ( dt.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(65);
      dt.d0.b[i] = dt.d0.b[i] + (double)(i+1)+I*(double)(i+1);
   }

   return 0;
}

int fnt5(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(69);
      dt->a[i] = dt->a[i] + (float)(i+1)+I*(float)(i+1);
      if ( dt->d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(73);
      dt->d1.a[i] = dt->d1.a[i] + (float)(i+1)+I*(float)(i+1);
      if ( dt->d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(77);
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(81);
      dt->b[i] = dt->b[i] + (double)(i+1)+I*(double)(i+1);
      if ( dt->d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(85);
      dt->d1.b[i] = dt->d1.b[i] + (double)(i+1)+I*(double)(i+1);
      if ( dt->d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(89);
      dt->d1.d0.b[i] = dt->d1.d0.b[i] + (double)(i+1)+I*(double)(i+1);
   }

   return 0;
}

int fnt6(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(93);
      dt.a[i] = dt.a[i] + (float)(i+1)+I*(float)(i+1);
      if ( dt.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(97);
      dt.d1.a[i] = dt.d1.a[i] + (float)(i+1)+I*(float)(i+1);
      if ( dt.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(101);
      dt.d1.d0.a[i] = dt.d1.d0.a[i] + (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(105);
      dt.b[i] = dt.b[i] + (double)(i+1)+I*(double)(i+1);
      if ( dt.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(109);
      dt.d1.b[i] = dt.d1.b[i] + (double)(i+1)+I*(double)(i+1);
      if ( dt.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(113);
      dt.d1.d0.b[i] = dt.d1.d0.b[i] + (double)(i+1)+I*(double)(i+1);
   }

   return 0;
}

int fnt7(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(117);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(121);
   }

   return 0;
}

int fnt7a(const struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(125);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(129);
   }

   return 0;
}

int fnt8(struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(133);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(137);
   }

   return 0;
}

int fnt8a(const struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(143);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(147);
   }

   return 0;
}

int fnt9(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(151);
      if ( dt->d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(155);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(159);
      if ( dt->d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(163);
   }

   return 0;
}

int fnt9a(const struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(167);
      if ( dt->d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(171);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(175);
      if ( dt->d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(179);
   }

   return 0;
}

int fnt10(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(183);
      if ( dt.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(187);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(191);
      if ( dt.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(195);
   }

   return 0;
}

int fnt10a(const struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(199);
      if ( dt.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(203);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(207);
      if ( dt.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(211);
   }

   return 0;
}

int fnt11(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(215);
      if ( dt->d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(219);
      if ( dt->d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(223);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(227);
      if ( dt->d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(231);
      if ( dt->d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(235);
   }

   return 0;
}

int fnt11a(const struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (float)(i+1)+I*(float)(i+1) ) exit(239);
      if ( dt->d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(243);
      if ( dt->d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(247);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != (double)(i+1)+I*(double)(i+1) ) exit(253);
      if ( dt->d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(257);
      if ( dt->d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(261);
   }

   return 0;
}

int fnt12(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(265);
      if ( dt.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(269);
      if ( dt.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(273);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(277);
      if ( dt.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(281);
      if ( dt.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(285);
   }

   return 0;
}

int fnt12a(const struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(289);
      if ( dt.d1.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(293);
      if ( dt.d1.d0.a[i] != (float)(i+1)+I*(float)(i+1) ) exit(297);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(301);
      if ( dt.d1.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(305);
      if ( dt.d1.d0.b[i] != (double)(i+1)+I*(double)(i+1) ) exit(309);
   }

   return 0;
}

int fnt13(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      dt->b[i] = dt->b[i] + (double)(i+1)+I*(double)(i+1);
   }

   return 0;
}

int fnt14(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + (float)(i+1)+I*(float)(i+1);
      dt->d0.a[i] = dt->d0.a[i] + (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      dt->b[i] = dt->b[i] + (double)(i+1)+I*(double)(i+1);
      dt->d0.b[i] = dt->d0.b[i] + (double)(i+1)+I*(double)(i+1);
   }

   return 0;
}

int fnt15(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + (float)(i+1)+I*(float)(i+1);
      dt->d1.a[i] = dt->d1.a[i] + (float)(i+1)+I*(float)(i+1);
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + (float)(i+1)+I*(float)(i+1);
   }

   for ( i = 0; i < 3; i++ ) {
      dt->b[i] = dt->b[i] + (double)(i+1)+I*(double)(i+1);
      dt->d1.b[i] = dt->d1.b[i] + (double)(i+1)+I*(double)(i+1);
      dt->d1.d0.b[i] = dt->d1.d0.b[i] + (double)(i+1)+I*(double)(i+1);
   }

   return 0;
}