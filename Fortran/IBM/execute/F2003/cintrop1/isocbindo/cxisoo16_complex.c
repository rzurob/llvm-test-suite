/*
        C code for testcase "fxisoo16.f" and "fxisoo17.f"
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

void sub1(struct dt0 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(21);

   dt->a = dt->a + 5.0l+I*5.0l;

}

void sub2(struct dt0 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(25);

   dt.a = dt.a + 5.0l+I*5.0l;

}

void sub3(struct dt1 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(29);
   if ( dt->d0.a != 5.0l+I*5.0l ) exit(33);

   dt->a = dt->a + 5.0l+I*5.0l;
   dt->d0.a = dt->d0.a + 5.0l+I*5.0l;

}

void sub4(struct dt1 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(37);
   if ( dt.d0.a != 5.0l+I*5.0l ) exit(41);

   dt.a = dt.a + 5.0l+I*5.0l;
   dt.d0.a = dt.d0.a + 5.0l+I*5.0l;

}

void sub5(struct dt2 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(45);
   if ( dt->d1.a != 5.0l+I*5.0l ) exit(49);
   if ( dt->d1.d0.a != 5.0l+I*5.0l ) exit(53);

   dt->a = dt->a + 5.0l+I*5.0l;
   dt->d1.a = dt->d1.a + 5.0l+I*5.0l;
   dt->d1.d0.a = dt->d1.d0.a + 5.0l+I*5.0l;

}

void sub6(struct dt2 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(57);
   if ( dt.d1.a != 5.0l+I*5.0l ) exit(61);
   if ( dt.d1.d0.a != 5.0l+I*5.0l ) exit(65);

   dt.a = dt.a + 5.0l+I*5.0l;
   dt.d1.a = dt.d1.a + 5.0l+I*5.0l;
   dt.d1.d0.a = dt.d1.d0.a + 5.0l+I*5.0l;

}

void sub7(struct dt0 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(69);

}

void sub7a(const struct dt0 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(73);

}

void sub8(struct dt0 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(77);

}

void sub8a(const struct dt0 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(81);

}

void sub9(struct dt1 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(85);
   if ( dt->d0.a != 5.0l+I*5.0l ) exit(89);

}

void sub9a(const struct dt1 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(93);
   if ( dt->d0.a != 5.0l+I*5.0l ) exit(97);

}

void sub10(struct dt1 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(101);
   if ( dt.d0.a != 5.0l+I*5.0l ) exit(105);

}

void sub10a(const struct dt1 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(109);
   if ( dt.d0.a != 5.0l+I*5.0l ) exit(113);

}

void sub11(struct dt2 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(117);
   if ( dt->d1.a != 5.0l+I*5.0l ) exit(121);
   if ( dt->d1.d0.a != 5.0l+I*5.0l ) exit(125);

}

void sub11a(const struct dt2 *dt) {

   if ( dt->a != 5.0l+I*5.0l ) exit(129);
   if ( dt->d1.a != 5.0l+I*5.0l ) exit(133);
   if ( dt->d1.d0.a != 5.0l+I*5.0l ) exit(137);

}

void sub12(struct dt2 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(143);
   if ( dt.d1.a != 5.0l+I*5.0l ) exit(147);
   if ( dt.d1.d0.a != 5.0l+I*5.0l ) exit(151);

}

void sub12a(const struct dt2 dt) {

   if ( dt.a != 5.0l+I*5.0l ) exit(155);
   if ( dt.d1.a != 5.0l+I*5.0l ) exit(159);
   if ( dt.d1.d0.a != 5.0l+I*5.0l ) exit(163);

}

void sub13(struct dt0 *dt) {

   dt->a = dt->a + 5.0l+I*5.0l;

}

void sub14(struct dt1 *dt) {

   dt->a = dt->a + 5.0l+I*5.0l;
   dt->d0.a = dt->d0.a + 5.0l+I*5.0l;

}

void sub15(struct dt2 *dt) {

   dt->a = dt->a + 5.0l+I*5.0l;
   dt->d1.a = dt->d1.a + 5.0l+I*5.0l;
   dt->d1.d0.a = dt->d1.d0.a + 5.0l+I*5.0l;

}

