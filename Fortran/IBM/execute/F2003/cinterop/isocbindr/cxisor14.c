/*
        C code for testcase "fxisor14.f" and "fxisor15.f"
*/

#include <stdio.h>
#include <stdlib.h>


struct dt0 {
   _Bool a;
};

struct dt1 {
   _Bool a;
   struct dt0 d0;
};

struct dt2 {
   _Bool a;
   struct dt1 d1;
};

_Bool fnt1(struct dt0 *dt) {

   if ( dt->a != 1 ) exit(21);

   dt->a = 0;

   return 0;
}

_Bool fnt2(struct dt0 dt) {

   if ( dt.a != 1 ) exit(23);

   dt.a = 0;

   return 0;
}

_Bool fnt3(struct dt1 *dt) {

   if ( dt->a != 1 ) exit(25);
   if ( dt->d0.a != 1 ) exit(27);

   dt->a = 0;
   dt->d0.a = 0;

   return 0;
}

_Bool fnt4(struct dt1 dt) {

   if ( dt.a != 1 ) exit(29);
   if ( dt.d0.a != 1 ) exit(31);

   dt.a = 0;
   dt.d0.a = 0;

   return 0;
}

_Bool fnt5(struct dt2 *dt) {

   if ( dt->a != 1 ) exit(33);
   if ( dt->d1.a != 1 ) exit(35);
   if ( dt->d1.d0.a != 1 ) exit(37);

   dt->a = 0;
   dt->d1.a = 0;
   dt->d1.d0.a = 0;

   return 0;
}

_Bool fnt6(struct dt2 dt) {

   if ( dt.a != 1 ) exit(39);
   if ( dt.d1.a != 1 ) exit(41);
   if ( dt.d1.d0.a != 1 ) exit(43);

   dt.a = 0;
   dt.d1.a = 0;
   dt.d1.d0.a = 0;

   return 0;
}

_Bool fnt7(struct dt0 *dt) {

   if ( dt->a != 1 ) exit(45);

   return 0;
}

_Bool fnt7a(const struct dt0 *dt) {

   if ( dt->a != 1 ) exit(47);

   return 0;
}

_Bool fnt8(struct dt0 dt) {

   if ( dt.a != 1 ) exit(49);

   return 0;
}

_Bool fnt8a(const struct dt0 dt) {

   if ( dt.a != 1 ) exit(51);

   return 0;
}

_Bool fnt9(struct dt1 *dt) {

   if ( dt->a != 1 ) exit(53);
   if ( dt->d0.a != 1 ) exit(55);

   return 0;
}

_Bool fnt9a(const struct dt1 *dt) {

   if ( dt->a != 1 ) exit(57);
   if ( dt->d0.a != 1 ) exit(59);

   return 0;
}

_Bool fnt10(struct dt1 dt) {

   if ( dt.a != 1 ) exit(61);
   if ( dt.d0.a != 1 ) exit(63);

   return 0;
}

_Bool fnt10a(const struct dt1 dt) {

   if ( dt.a != 1 ) exit(65);
   if ( dt.d0.a != 1 ) exit(67);

   return 0;
}

_Bool fnt11(struct dt2 *dt) {

   if ( dt->a != 1 ) exit(69);
   if ( dt->d1.a != 1 ) exit(71);
   if ( dt->d1.d0.a != 1 ) exit(73);

   return 0;
}

_Bool fnt11a(const struct dt2 *dt) {

   if ( dt->a != 1 ) exit(75);
   if ( dt->d1.a != 1 ) exit(77);
   if ( dt->d1.d0.a != 1 ) exit(79);

   return 0;
}

_Bool fnt12(struct dt2 dt) {

   if ( dt.a != 1 ) exit(81);
   if ( dt.d1.a != 1 ) exit(83);
   if ( dt.d1.d0.a != 1 ) exit(85);

   return 0;
}

_Bool fnt12a(const struct dt2 dt) {

   if ( dt.a != 1 ) exit(87);
   if ( dt.d1.a != 1 ) exit(89);
   if ( dt.d1.d0.a != 1 ) exit(91);

   return 0;
}

_Bool fnt13(struct dt0 *dt) {

   dt->a = 0;

   return 0;
}

_Bool fnt14(struct dt1 *dt) {

   dt->a = 0;
   dt->d0.a = 0;

   return 0;
}

_Bool fnt15(struct dt2 *dt) {

   dt->a = 0;
   dt->d1.a = 0;
   dt->d1.d0.a = 0;

   return 0;
}

