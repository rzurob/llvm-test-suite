/*
        C code for testcase "fxisoi14.f" and "fxisoi15.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

struct dt0 {
   int_fast16_t a;
};

struct dt1 {
   int_fast16_t a;
   struct dt0 d0;
};

struct dt2 {
   int_fast16_t a;
   struct dt1 d1;
};

int_fast16_t fnt1(struct dt0 *dt) {

   if ( dt->a != 5 ) exit(21);

   dt->a = dt->a + 5;

   return 0;
}

int_fast16_t fnt2(struct dt0 dt) {

   if ( dt.a != 5 ) exit(23);

   dt.a = dt.a + 5;

   return 0;
}

int_fast16_t fnt3(struct dt1 *dt) {

   if ( dt->a != 5 ) exit(25);
   if ( dt->d0.a != 5 ) exit(27);

   dt->a = dt->a + 5;
   dt->d0.a = dt->d0.a + 5;

   return 0;
}

int_fast16_t fnt4(struct dt1 dt) {

   if ( dt.a != 5 ) exit(29);
   if ( dt.d0.a != 5 ) exit(31);

   dt.a = dt.a + 5;
   dt.d0.a = dt.d0.a + 5;

   return 0;
}

int_fast16_t fnt5(struct dt2 *dt) {

   if ( dt->a != 5 ) exit(33);
   if ( dt->d1.a != 5 ) exit(35);
   if ( dt->d1.d0.a != 5 ) exit(37);

   dt->a = dt->a + 5;
   dt->d1.a = dt->d1.a + 5;
   dt->d1.d0.a = dt->d1.d0.a + 5;

   return 0;
}

int_fast16_t fnt6(struct dt2 dt) {

   if ( dt.a != 5 ) exit(39);
   if ( dt.d1.a != 5 ) exit(41);
   if ( dt.d1.d0.a != 5 ) exit(43);

   dt.a = dt.a + 5;
   dt.d1.a = dt.d1.a + 5;
   dt.d1.d0.a = dt.d1.d0.a + 5;

   return 0;
}

int_fast16_t fnt7(struct dt0 *dt) {

   if ( dt->a != 5 ) exit(45);

   return 0;
}

int_fast16_t fnt7a(const struct dt0 *dt) {

   if ( dt->a != 5 ) exit(47);

   return 0;
}

int_fast16_t fnt8(struct dt0 dt) {

   if ( dt.a != 5 ) exit(49);

   return 0;
}

int_fast16_t fnt8a(const struct dt0 dt) {

   if ( dt.a != 5 ) exit(51);

   return 0;
}

int_fast16_t fnt9(struct dt1 *dt) {

   if ( dt->a != 5 ) exit(53);
   if ( dt->d0.a != 5 ) exit(55);

   return 0;
}

int_fast16_t fnt9a(const struct dt1 *dt) {

   if ( dt->a != 5 ) exit(57);
   if ( dt->d0.a != 5 ) exit(59);

   return 0;
}

int_fast16_t fnt10(struct dt1 dt) {

   if ( dt.a != 5 ) exit(61);
   if ( dt.d0.a != 5 ) exit(63);

   return 0;
}

int_fast16_t fnt10a(const struct dt1 dt) {

   if ( dt.a != 5 ) exit(65);
   if ( dt.d0.a != 5 ) exit(67);

   return 0;
}

int_fast16_t fnt11(struct dt2 *dt) {

   if ( dt->a != 5 ) exit(69);
   if ( dt->d1.a != 5 ) exit(71);
   if ( dt->d1.d0.a != 5 ) exit(73);

   return 0;
}

int_fast16_t fnt11a(const struct dt2 *dt) {

   if ( dt->a != 5 ) exit(75);
   if ( dt->d1.a != 5 ) exit(77);
   if ( dt->d1.d0.a != 5 ) exit(79);

   return 0;
}

int_fast16_t fnt12(struct dt2 dt) {

   if ( dt.a != 5 ) exit(81);
   if ( dt.d1.a != 5 ) exit(83);
   if ( dt.d1.d0.a != 5 ) exit(85);

   return 0;
}

int_fast16_t fnt12a(const struct dt2 dt) {

   if ( dt.a != 5 ) exit(87);
   if ( dt.d1.a != 5 ) exit(89);
   if ( dt.d1.d0.a != 5 ) exit(91);

   return 0;
}

int_fast16_t fnt13(struct dt0 *dt) {

   dt->a = dt->a + 5;

   return 0;
}

int_fast16_t fnt14(struct dt1 *dt) {

   dt->a = dt->a + 5;
   dt->d0.a = dt->d0.a + 5;

   return 0;
}

int_fast16_t fnt15(struct dt2 *dt) {

   dt->a = dt->a + 5;
   dt->d1.a = dt->d1.a + 5;
   dt->d1.d0.a = dt->d1.d0.a + 5;

   return 0;
}

