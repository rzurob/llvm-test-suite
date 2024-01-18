/*
        C code for testcase "fxisoi16.f" and "fxisoi17.f"
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

void sub1(struct dt0 *dt) {

   if ( dt->a != 5 ) exit(21);

   dt->a = dt->a + 5;

}

void sub2(struct dt0 dt) {

   if ( dt.a != 5 ) exit(23);

   dt.a = dt.a + 5;

}

void sub3(struct dt1 *dt) {

   if ( dt->a != 5 ) exit(25);
   if ( dt->d0.a != 5 ) exit(27);

   dt->a = dt->a + 5;
   dt->d0.a = dt->d0.a + 5;

}

void sub4(struct dt1 dt) {

   if ( dt.a != 5 ) exit(29);
   if ( dt.d0.a != 5 ) exit(31);

   dt.a = dt.a + 5;
   dt.d0.a = dt.d0.a + 5;

}

void sub5(struct dt2 *dt) {

   if ( dt->a != 5 ) exit(33);
   if ( dt->d1.a != 5 ) exit(35);
   if ( dt->d1.d0.a != 5 ) exit(37);

   dt->a = dt->a + 5;
   dt->d1.a = dt->d1.a + 5;
   dt->d1.d0.a = dt->d1.d0.a + 5;

}

void sub6(struct dt2 dt) {

   if ( dt.a != 5 ) exit(39);
   if ( dt.d1.a != 5 ) exit(41);
   if ( dt.d1.d0.a != 5 ) exit(43);

   dt.a = dt.a + 5;
   dt.d1.a = dt.d1.a + 5;
   dt.d1.d0.a = dt.d1.d0.a + 5;

}

void sub7(struct dt0 *dt) {

   if ( dt->a != 5 ) exit(45);

}

void sub7a(const struct dt0 *dt) {

   if ( dt->a != 5 ) exit(47);

}

void sub8(struct dt0 dt) {

   if ( dt.a != 5 ) exit(49);

}

void sub8a(const struct dt0 dt) {

   if ( dt.a != 5 ) exit(51);

}

void sub9(struct dt1 *dt) {

   if ( dt->a != 5 ) exit(53);
   if ( dt->d0.a != 5 ) exit(55);

}

void sub9a(const struct dt1 *dt) {

   if ( dt->a != 5 ) exit(57);
   if ( dt->d0.a != 5 ) exit(59);

}

void sub10(struct dt1 dt) {

   if ( dt.a != 5 ) exit(61);
   if ( dt.d0.a != 5 ) exit(63);

}

void sub10a(const struct dt1 dt) {

   if ( dt.a != 5 ) exit(65);
   if ( dt.d0.a != 5 ) exit(67);

}

void sub11(struct dt2 *dt) {

   if ( dt->a != 5 ) exit(69);
   if ( dt->d1.a != 5 ) exit(71);
   if ( dt->d1.d0.a != 5 ) exit(73);

}

void sub11a(const struct dt2 *dt) {

   if ( dt->a != 5 ) exit(75);
   if ( dt->d1.a != 5 ) exit(77);
   if ( dt->d1.d0.a != 5 ) exit(79);

}

void sub12(struct dt2 dt) {

   if ( dt.a != 5 ) exit(81);
   if ( dt.d1.a != 5 ) exit(83);
   if ( dt.d1.d0.a != 5 ) exit(85);

}

void sub12a(const struct dt2 dt) {

   if ( dt.a != 5 ) exit(87);
   if ( dt.d1.a != 5 ) exit(89);
   if ( dt.d1.d0.a != 5 ) exit(91);

}

void sub13(struct dt0 *dt) {

   dt->a = dt->a + 5;

}

void sub14(struct dt1 *dt) {

   dt->a = dt->a + 5;
   dt->d0.a = dt->d0.a + 5;

}

void sub15(struct dt2 *dt) {

   dt->a = dt->a + 5;
   dt->d1.a = dt->d1.a + 5;
   dt->d1.d0.a = dt->d1.d0.a + 5;

}

