/*
        C code for testcase "fxisom14.f" and "fxisom15.f"
*/

#include <stdio.h>
#include <stdlib.h>

struct dt0 {
   long double a;
};

struct dt1 {
   long double a;
   struct dt0 d0;
};

struct dt2 {
   long double a;
   struct dt1 d1;
};

long double fnt1(struct dt0 *dt) {

   if ( dt->a != 5.0l ) exit(21);

   dt->a = dt->a + 5.0l;

   return 0;
}

long double fnt2(struct dt0 dt) {

   if ( dt.a != 5.0l ) exit(23);

   dt.a = dt.a + 5.0l;

   return 0;
}

long double fnt3(struct dt1 *dt) {

   if ( dt->a != 5.0l ) exit(25);
   if ( dt->d0.a != 5.0l ) exit(27);

   dt->a = dt->a + 5.0l;
   dt->d0.a = dt->d0.a + 5.0l;

   return 0;
}

long double fnt4(struct dt1 dt) {

   if ( dt.a != 5.0l ) exit(29);
   if ( dt.d0.a != 5.0l ) exit(31);

   dt.a = dt.a + 5.0l;
   dt.d0.a = dt.d0.a + 5.0l;

   return 0;
}

long double fnt5(struct dt2 *dt) {

   if ( dt->a != 5.0l ) exit(33);
   if ( dt->d1.a != 5.0l ) exit(35);
   if ( dt->d1.d0.a != 5.0l ) exit(37);

   dt->a = dt->a + 5.0l;
   dt->d1.a = dt->d1.a + 5.0l;
   dt->d1.d0.a = dt->d1.d0.a + 5.0l;

   return 0;
}

long double fnt6(struct dt2 dt) {

   if ( dt.a != 5.0l ) exit(39);
   if ( dt.d1.a != 5.0l ) exit(41);
   if ( dt.d1.d0.a != 5.0l ) exit(43);

   dt.a = dt.a + 5.0l;
   dt.d1.a = dt.d1.a + 5.0l;
   dt.d1.d0.a = dt.d1.d0.a + 5.0l;

   return 0;
}

long double fnt7(struct dt0 *dt) {

   if ( dt->a != 5.0l ) exit(45);

   return 0;
}

long double fnt7a(const struct dt0 *dt) {

   if ( dt->a != 5.0l ) exit(47);

   return 0;
}

long double fnt8(struct dt0 dt) {

   if ( dt.a != 5.0l ) exit(49);

   return 0;
}

long double fnt8a(const struct dt0 dt) {

   if ( dt.a != 5.0l ) exit(51);

   return 0;
}

long double fnt9(struct dt1 *dt) {

   if ( dt->a != 5.0l ) exit(53);
   if ( dt->d0.a != 5.0l ) exit(55);

   return 0;
}

long double fnt9a(const struct dt1 *dt) {

   if ( dt->a != 5.0l ) exit(57);
   if ( dt->d0.a != 5.0l ) exit(59);

   return 0;
}

long double fnt10(struct dt1 dt) {

   if ( dt.a != 5.0l ) exit(61);
   if ( dt.d0.a != 5.0l ) exit(63);

   return 0;
}

long double fnt10a(const struct dt1 dt) {

   if ( dt.a != 5.0l ) exit(65);
   if ( dt.d0.a != 5.0l ) exit(67);

   return 0;
}

long double fnt11(struct dt2 *dt) {

   if ( dt->a != 5.0l ) exit(69);
   if ( dt->d1.a != 5.0l ) exit(71);
   if ( dt->d1.d0.a != 5.0l ) exit(73);

   return 0;
}

long double fnt11a(const struct dt2 *dt) {

   if ( dt->a != 5.0l ) exit(75);
   if ( dt->d1.a != 5.0l ) exit(77);
   if ( dt->d1.d0.a != 5.0l ) exit(79);

   return 0;
}

long double fnt12(struct dt2 dt) {

   if ( dt.a != 5.0l ) exit(81);
   if ( dt.d1.a != 5.0l ) exit(83);
   if ( dt.d1.d0.a != 5.0l ) exit(85);

   return 0;
}

long double fnt12a(const struct dt2 dt) {

   if ( dt.a != 5.0l ) exit(87);
   if ( dt.d1.a != 5.0l ) exit(89);
   if ( dt.d1.d0.a != 5.0l ) exit(91);

   return 0;
}

long double fnt13(struct dt0 *dt) {

   dt->a = dt->a + 5.0l;

   return 0;
}

long double fnt14(struct dt1 *dt) {

   dt->a = dt->a + 5.0l;
   dt->d0.a = dt->d0.a + 5.0l;

   return 0;
}

long double fnt15(struct dt2 *dt) {

   dt->a = dt->a + 5.0l;
   dt->d1.a = dt->d1.a + 5.0l;
   dt->d1.d0.a = dt->d1.d0.a + 5.0l;

   return 0;
}

