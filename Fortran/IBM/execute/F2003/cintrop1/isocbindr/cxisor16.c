/*
        C code for testcase "fxisor16.f" and "fxisor17.f"
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

void sub1(struct dt0 *dt) {

   if ( dt->a != 1 ) exit(21);

   dt->a = 0;

}

void sub2(struct dt0 dt) {

   if ( dt.a != 1 ) exit(23);

   dt.a = 0;

}

void sub3(struct dt1 *dt) {

   if ( dt->a != 1 ) exit(25);
   if ( dt->d0.a != 1 ) exit(27);

   dt->a = 0;
   dt->d0.a = 0;

}

void sub4(struct dt1 dt) {

   if ( dt.a != 1 ) exit(29);
   if ( dt.d0.a != 1 ) exit(31);

   dt.a = 0;
   dt.d0.a = 0;

}

void sub5(struct dt2 *dt) {

   if ( dt->a != 1 ) exit(33);
   if ( dt->d1.a != 1 ) exit(35);
   if ( dt->d1.d0.a != 1 ) exit(37);

   dt->a = 0;
   dt->d1.a = 0;
   dt->d1.d0.a = 0;

}

void sub6(struct dt2 dt) {

   if ( dt.a != 1 ) exit(39);
   if ( dt.d1.a != 1 ) exit(41);
   if ( dt.d1.d0.a != 1 ) exit(43);

   dt.a = 0;
   dt.d1.a = 0;
   dt.d1.d0.a = 0;

}

void sub7(struct dt0 *dt) {

   if ( dt->a != 1 ) exit(45);

}

void sub7a(const struct dt0 *dt) {

   if ( dt->a != 1 ) exit(47);

}

void sub8(struct dt0 dt) {

   if ( dt.a != 1 ) exit(49);

}

void sub8a(const struct dt0 dt) {

   if ( dt.a != 1 ) exit(51);

}

void sub9(struct dt1 *dt) {

   if ( dt->a != 1 ) exit(53);
   if ( dt->d0.a != 1 ) exit(55);

}

void sub9a(const struct dt1 *dt) {

   if ( dt->a != 1 ) exit(57);
   if ( dt->d0.a != 1 ) exit(59);

}

void sub10(struct dt1 dt) {

   if ( dt.a != 1 ) exit(61);
   if ( dt.d0.a != 1 ) exit(63);

}

void sub10a(const struct dt1 dt) {

   if ( dt.a != 1 ) exit(65);
   if ( dt.d0.a != 1 ) exit(67);

}

void sub11(struct dt2 *dt) {

   if ( dt->a != 1 ) exit(69);
   if ( dt->d1.a != 1 ) exit(71);
   if ( dt->d1.d0.a != 1 ) exit(73);

}

void sub11a(const struct dt2 *dt) {

   if ( dt->a != 1 ) exit(75);
   if ( dt->d1.a != 1 ) exit(77);
   if ( dt->d1.d0.a != 1 ) exit(79);

}

void sub12(struct dt2 dt) {

   if ( dt.a != 1 ) exit(81);
   if ( dt.d1.a != 1 ) exit(83);
   if ( dt.d1.d0.a != 1 ) exit(85);

}

void sub12a(const struct dt2 dt) {

   if ( dt.a != 1 ) exit(87);
   if ( dt.d1.a != 1 ) exit(89);
   if ( dt.d1.d0.a != 1 ) exit(91);

}

void sub13(struct dt0 *dt) {

   dt->a = 0;

}

void sub14(struct dt1 *dt) {

   dt->a = 0;
   dt->d0.a = 0;

}

void sub15(struct dt2 *dt) {

   dt->a = 0;
   dt->d1.a = 0;
   dt->d1.d0.a = 0;

}

