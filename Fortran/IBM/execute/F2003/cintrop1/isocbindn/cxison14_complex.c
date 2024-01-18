/*
        C code for testcase "fxison14.f" and "fxison15.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

struct dt0 {
   float _Complex a;
   double _Complex b;
};

struct dt1 {
   float _Complex a;
   double _Complex b;
   struct dt0 d0;
};

struct dt2 {
   float _Complex a;
   double _Complex b;
   struct dt1 d1;
};

float _Complex fnt1(struct dt0 *dt) {

   if ( dt->a != 5.0f+I*5.0f ) exit(21);
   if ( dt->b != 10.0+I*10.0 ) exit(25);

   dt->a = dt->a + 5.0f+I*5.0f;
   dt->b = dt->b + 10.0+I*10.0;

   return 0;
}

float _Complex fnt2(struct dt0 dt) {

   if ( dt.a != 5.0f+I*5.0f ) exit(29);
   if ( dt.b != 10.0+I*10.0 ) exit(33);

   dt.a = dt.a + 5.0f+I*5.0f;
   dt.b = dt.b + 10.0+I*10.0;

   return 0;
}

float _Complex fnt3(struct dt1 *dt) {

   if ( dt->a != 5.0f+I*5.0f ) exit(37);
   if ( dt->b != 10.0+I*10.0 ) exit(41);
   if ( dt->d0.a != 5.0f+I*5.0f ) exit(45);
   if ( dt->d0.b != 10.0+I*10.0 ) exit(49);

   dt->a = dt->a + 5.0f+I*5.0f;
   dt->b = dt->b + 10.0+I*10.0;
   dt->d0.a = dt->d0.a + 5.0f+I*5.0f;
   dt->d0.b = dt->d0.b + 10.0+I*10.0;

   return 0;
}

float _Complex fnt4(struct dt1 dt) {

   if ( dt.a != 5.0f+I*5.0f ) exit(53);
   if ( dt.b != 10.0+I*10.0 ) exit(57);
   if ( dt.d0.a != 5.0f+I*5.0f ) exit(61);
   if ( dt.d0.b != 10.0+I*10.0 ) exit(65);

   dt.a = dt.a + 5.0f+I*5.0f;
   dt.b = dt.b + 10.0+I*10.0;
   dt.d0.a = dt.d0.a + 5.0f+I*5.0f;
   dt.d0.b = dt.d0.b + 10.0+I*10.0;

   return 0;
}

float _Complex fnt5(struct dt2 *dt) {

   if ( dt->a != 5.0f+I*5.0f ) exit(69);
   if ( dt->b != 10.0+I*10.0 ) exit(73);
   if ( dt->d1.a != 5.0f+I*5.0f ) exit(77);
   if ( dt->d1.b != 10.0+I*10.0 ) exit(81);
   if ( dt->d1.d0.a != 5.0f+I*5.0f ) exit(85);
   if ( dt->d1.d0.b != 10.0+I*10.0 ) exit(89);

   dt->a = dt->a + 5.0f+I*5.0f;
   dt->b = dt->b + 10.0+I*10.0;
   dt->d1.a = dt->d1.a + 5.0f+I*5.0f;
   dt->d1.b = dt->d1.b + 10.0+I*10.0;
   dt->d1.d0.a = dt->d1.d0.a + 5.0f+I*5.0f;
   dt->d1.d0.b = dt->d1.d0.b + 10.0+I*10.0;

   return 0;
}

float _Complex fnt6(struct dt2 dt) {

   if ( dt.a != 5.0f+I*5.0f ) exit(93);
   if ( dt.b != 10.0+I*10.0 ) exit(97);
   if ( dt.d1.a != 5.0f+I*5.0f ) exit(101);
   if ( dt.d1.b != 10.0+I*10.0 ) exit(105);
   if ( dt.d1.d0.a != 5.0f+I*5.0f ) exit(109);
   if ( dt.d1.d0.b != 10.0+I*10.0 ) exit(113);

   dt.a = dt.a + 5.0f+I*5.0f;
   dt.b = dt.b + 10.0+I*10.0;
   dt.d1.a = dt.d1.a + 5.0f+I*5.0f;
   dt.d1.b = dt.d1.b + 10.0+I*10.0;
   dt.d1.d0.a = dt.d1.d0.a + 5.0f+I*5.0f;
   dt.d1.d0.b = dt.d1.d0.b + 10.0+I*10.0;

   return 0;
}

float _Complex fnt7(struct dt0 *dt) {

   if ( dt->a != 5.0f+I*5.0f ) exit(117);
   if ( dt->b != 10.0+I*10.0 ) exit(121);

   return 0;
}

float _Complex fnt7a(const struct dt0 *dt) {

   if ( dt->a != 5.0f+I*5.0f ) exit(125);
   if ( dt->b != 10.0+I*10.0 ) exit(129);

   return 0;
}

float _Complex fnt8(struct dt0 dt) {

   if ( dt.a != 5.0f+I*5.0f ) exit(133);
   if ( dt.b != 10.0+I*10.0 ) exit(137);

   return 0;
}

float _Complex fnt8a(const struct dt0 dt) {

   if ( dt.a != 5.0f+I*5.0f ) exit(143);
   if ( dt.b != 10.0+I*10.0 ) exit(147);

   return 0;
}

float _Complex fnt9(struct dt1 *dt) {

   if ( dt->a != 5.0f+I*5.0f ) exit(151);
   if ( dt->b != 10.0+I*10.0 ) exit(155);
   if ( dt->d0.a != 5.0f+I*5.0f ) exit(159);
   if ( dt->d0.b != 10.0+I*10.0 ) exit(163);

   return 0;
}

float _Complex fnt9a(const struct dt1 *dt) {

   if ( dt->a != 5.0f+I*5.0f ) exit(167);
   if ( dt->b != 10.0+I*10.0 ) exit(171);
   if ( dt->d0.a != 5.0f+I*5.0f ) exit(175);
   if ( dt->d0.b != 10.0+I*10.0 ) exit(179);

   return 0;
}

float _Complex fnt10(struct dt1 dt) {

   if ( dt.a != 5.0f+I*5.0f ) exit(183);
   if ( dt.b != 10.0+I*10.0 ) exit(187);
   if ( dt.d0.a != 5.0f+I*5.0f ) exit(191);
   if ( dt.d0.b != 10.0+I*10.0 ) exit(195);

   return 0;
}

float _Complex fnt10a(const struct dt1 dt) {

   if ( dt.a != 5.0f+I*5.0f ) exit(199);
   if ( dt.b != 10.0+I*10.0 ) exit(203);
   if ( dt.d0.a != 5.0f+I*5.0f ) exit(207);
   if ( dt.d0.b != 10.0+I*10.0 ) exit(211);

   return 0;
}

float _Complex fnt11(struct dt2 *dt) {

   if ( dt->a != 5.0f+I*5.0f ) exit(215);
   if ( dt->b != 10.0+I*10.0 ) exit(219);
   if ( dt->d1.a != 5.0f+I*5.0f ) exit(223);
   if ( dt->d1.b != 10.0+I*10.0 ) exit(227);
   if ( dt->d1.d0.a != 5.0f+I*5.0f ) exit(231);
   if ( dt->d1.d0.b != 10.0+I*10.0 ) exit(235);

   return 0;
}

float _Complex fnt11a(const struct dt2 *dt) {

   if ( dt->a != 5.0f+I*5.0f ) exit(239);
   if ( dt->b != 10.0+I*10.0 ) exit(243);
   if ( dt->d1.a != 5.0f+I*5.0f ) exit(247);
   if ( dt->d1.b != 10.0+I*10.0 ) exit(253);
   if ( dt->d1.d0.a != 5.0f+I*5.0f ) exit(257);
   if ( dt->d1.d0.b != 10.0+I*10.0 ) exit(261);

   return 0;
}

float _Complex fnt12(struct dt2 dt) {

   if ( dt.a != 5.0f+I*5.0f ) exit(265);
   if ( dt.b != 10.0+I*10.0 ) exit(269);
   if ( dt.d1.a != 5.0f+I*5.0f ) exit(273);
   if ( dt.d1.b != 10.0+I*10.0 ) exit(277);
   if ( dt.d1.d0.a != 5.0f+I*5.0f ) exit(281);
   if ( dt.d1.d0.b != 10.0+I*10.0 ) exit(285);

   return 0;
}

float _Complex fnt12a(const struct dt2 dt) {

   if ( dt.a != 5.0f+I*5.0f ) exit(289);
   if ( dt.b != 10.0+I*10.0 ) exit(293);
   if ( dt.d1.a != 5.0f+I*5.0f ) exit(297);
   if ( dt.d1.b != 10.0+I*10.0 ) exit(301);
   if ( dt.d1.d0.a != 5.0f+I*5.0f ) exit(305);
   if ( dt.d1.d0.b != 10.0+I*10.0 ) exit(309);

   return 0;
}

float _Complex fnt13(struct dt0 *dt) {

   dt->a = dt->a + 5.0f+I*5.0f;
   dt->b = dt->b + 10.0+I*10.0;

   return 0;
}

float _Complex fnt14(struct dt1 *dt) {

   dt->a = dt->a + 5.0f+I*5.0f;
   dt->b = dt->b + 10.0+I*10.0;
   dt->d0.a = dt->d0.a + 5.0f+I*5.0f;
   dt->d0.b = dt->d0.b + 10.0+I*10.0;

   return 0;
}

float _Complex fnt15(struct dt2 *dt) {

   dt->a = dt->a + 5.0f+I*5.0f;
   dt->b = dt->b + 10.0+I*10.0;
   dt->d1.a = dt->d1.a + 5.0f+I*5.0f;
   dt->d1.b = dt->d1.b + 10.0+I*10.0;
   dt->d1.d0.a = dt->d1.d0.a + 5.0f+I*5.0f;
   dt->d1.d0.b = dt->d1.d0.b + 10.0+I*10.0;

   return 0;
}

