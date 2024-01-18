/*
        C code for testcase "fxisoq14.f" and "fxisoq15.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

struct dt0 {
   long double _Complex a;
};

struct dt1 {
   long double _Complex a;
   struct dt0 d0;
};

struct dt2 {
   long double _Complex a;
   struct dt1 d1;
};

long double _Complex fnt1(struct dt0 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(21);

   dt->a = dt->a + 5.0l+I*5.0l;

   return 0;
}

long double _Complex fnt2(struct dt0 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(25);

   dt.a = dt.a + 5.0l+I*5.0l;

   return 0;
}

long double _Complex fnt3(struct dt1 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(29);
   if ( dt->d0.a != 5.0l+I*5.0l ) exit(33);

   dt->a = dt->a + 5.0l+I*5.0l;
   dt->d0.a = dt->d0.a + 5.0l+I*5.0l;

   return 0;
}

long double _Complex fnt4(struct dt1 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(37);
   if ( dt.d0.a != 5.0l+I*5.0l ) exit(41);

   dt.a = dt.a + 5.0l+I*5.0l;
   dt.d0.a = dt.d0.a + 5.0l+I*5.0l;

   return 0;
}

long double _Complex fnt5(struct dt2 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(45);
   if ( dt->d1.a != 5.0l+I*5.0l ) exit(49);
   if ( dt->d1.d0.a != 5.0l+I*5.0l ) exit(53);

   dt->a = dt->a + 5.0l+I*5.0l;
   dt->d1.a = dt->d1.a + 5.0l+I*5.0l;
   dt->d1.d0.a = dt->d1.d0.a + 5.0l+I*5.0l;

   return 0;
}

long double _Complex fnt6(struct dt2 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(57);
   if ( dt.d1.a != 5.0l+I*5.0l ) exit(61);
   if ( dt.d1.d0.a != 5.0l+I*5.0l ) exit(65);

   dt.a = dt.a + 5.0l+I*5.0l;
   dt.d1.a = dt.d1.a + 5.0l+I*5.0l;
   dt.d1.d0.a = dt.d1.d0.a + 5.0l+I*5.0l;

   return 0;
}

long double _Complex fnt7(struct dt0 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(69);

   return 0;
}

long double _Complex fnt7a(const struct dt0 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(73);

   return 0;
}

long double _Complex fnt8(struct dt0 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(77);

   return 0;
}

long double _Complex fnt8a(const struct dt0 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(81);

   return 0;
}

long double _Complex fnt9(struct dt1 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(85);
   if ( dt->d0.a != 5.0l+I*5.0l ) exit(89);

   return 0;
}

long double _Complex fnt9a(const struct dt1 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(93);
   if ( dt->d0.a != 5.0l+I*5.0l ) exit(97);

   return 0;
}

long double _Complex fnt10(struct dt1 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(101);
   if ( dt.d0.a != 5.0l+I*5.0l ) exit(105);

   return 0;
}

long double _Complex fnt10a(const struct dt1 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(109);
   if ( dt.d0.a != 5.0l+I*5.0l ) exit(113);

   return 0;
}

long double _Complex fnt11(struct dt2 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(117);
   if ( dt->d1.a != 5.0l+I*5.0l ) exit(121);
   if ( dt->d1.d0.a != 5.0l+I*5.0l ) exit(125);

   return 0;
}

long double _Complex fnt11a(const struct dt2 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(129);
   if ( dt->d1.a != 5.0l+I*5.0l ) exit(133);
   if ( dt->d1.d0.a != 5.0l+I*5.0l ) exit(137);

   return 0;
}

long double _Complex fnt12(struct dt2 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(143);
   if ( dt.d1.a != 5.0l+I*5.0l ) exit(147);
   if ( dt.d1.d0.a != 5.0l+I*5.0l ) exit(151);

   return 0;
}

long double _Complex fnt12a(const struct dt2 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(155);
   if ( dt.d1.a != 5.0l+I*5.0l ) exit(159);
   if ( dt.d1.d0.a != 5.0l+I*5.0l ) exit(163);

   return 0;
}

long double _Complex fnt13(struct dt0 *dt) {

   dt->a = dt->a + 5.0l+I*5.0l;

   return 0;
}

long double _Complex fnt14(struct dt1 *dt) {

   dt->a = dt->a + 5.0l+I*5.0l;
   dt->d0.a = dt->d0.a + 5.0l+I*5.0l;

   return 0;
}

long double _Complex fnt15(struct dt2 *dt) {

   dt->a = dt->a + 5.0l+I*5.0l;
   dt->d1.a = dt->d1.a + 5.0l+I*5.0l;
   dt->d1.d0.a = dt->d1.d0.a + 5.0l+I*5.0l;

   return 0;
}

