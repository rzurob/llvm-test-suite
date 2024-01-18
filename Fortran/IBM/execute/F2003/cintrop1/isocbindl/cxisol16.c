/*
        C code for testcase "fxisol16.f" and "fxisol17.f"
*/

#include <stdio.h>
#include <stdlib.h>


struct dt0 {
   float a;
   double b;
};

struct dt1 {
   float a;
   double b;
   struct dt0 d0;
};

struct dt2 {
   float a;
   double b;
   struct dt1 d1;
};

void sub1(struct dt0 *dt) {

   if ( dt->a != 5.0f ) exit(21);
   if ( dt->b != 10.0 ) exit(23);

   dt->a = dt->a + 5.0f;
   dt->b = dt->b + 10.0;

}

void sub2(struct dt0 dt) {

   if ( dt.a != 5.0f ) exit(25);
   if ( dt.b != 10.0 ) exit(27);

   dt.a = dt.a + 5.0f;
   dt.b = dt.b + 10.0;

}

void sub3(struct dt1 *dt) {

   if ( dt->a != 5.0f ) exit(29);
   if ( dt->b != 10.0 ) exit(31);
   if ( dt->d0.a != 5.0f ) exit(33);
   if ( dt->d0.b != 10.0 ) exit(35);

   dt->a = dt->a + 5.0f;
   dt->b = dt->b + 10.0;
   dt->d0.a = dt->d0.a + 5.0f;
   dt->d0.b = dt->d0.b + 10.0;

}

void sub4(struct dt1 dt) {

   if ( dt.a != 5.0f ) exit(37);
   if ( dt.b != 10.0 ) exit(39);
   if ( dt.d0.a != 5.0f ) exit(41);
   if ( dt.d0.b != 10.0 ) exit(43);

   dt.a = dt.a + 5.0f;
   dt.b = dt.b + 10.0;
   dt.d0.a = dt.d0.a + 5.0f;
   dt.d0.b = dt.d0.b + 10.0;

}

void sub5(struct dt2 *dt) {

   if ( dt->a != 5.0f ) exit(45);
   if ( dt->b != 10.0 ) exit(47);
   if ( dt->d1.a != 5.0f ) exit(49);
   if ( dt->d1.b != 10.0 ) exit(51);
   if ( dt->d1.d0.a != 5.0f ) exit(53);
   if ( dt->d1.d0.b != 10.0 ) exit(55);

   dt->a = dt->a + 5.0f;
   dt->b = dt->b + 10.0;
   dt->d1.a = dt->d1.a + 5.0f;
   dt->d1.b = dt->d1.b + 10.0;
   dt->d1.d0.a = dt->d1.d0.a + 5.0f;
   dt->d1.d0.b = dt->d1.d0.b + 10.0;

}

void sub6(struct dt2 dt) {

   if ( dt.a != 5.0f ) exit(57);
   if ( dt.b != 10.0 ) exit(59);
   if ( dt.d1.a != 5.0f ) exit(61);
   if ( dt.d1.b != 10.0 ) exit(63);
   if ( dt.d1.d0.a != 5.0f ) exit(65);
   if ( dt.d1.d0.b != 10.0 ) exit(67);

   dt.a = dt.a + 5.0f;
   dt.b = dt.b + 10.0;
   dt.d1.a = dt.d1.a + 5.0f;
   dt.d1.b = dt.d1.b + 10.0;
   dt.d1.d0.a = dt.d1.d0.a + 5.0f;
   dt.d1.d0.b = dt.d1.d0.b + 10.0;

}

void sub7(struct dt0 *dt) {

   if ( dt->a != 5.0f ) exit(69);
   if ( dt->b != 10.0 ) exit(71);

}

void sub7a(const struct dt0 *dt) {

   if ( dt->a != 5.0f ) exit(73);
   if ( dt->b != 10.0 ) exit(75);

}

void sub8(struct dt0 dt) {

   if ( dt.a != 5.0f ) exit(77);
   if ( dt.b != 10.0 ) exit(79);

}

void sub8a(const struct dt0 dt) {

   if ( dt.a != 5.0f ) exit(81);
   if ( dt.b != 10.0 ) exit(83);

}

void sub9(struct dt1 *dt) {

   if ( dt->a != 5.0f ) exit(85);
   if ( dt->b != 10.0 ) exit(87);
   if ( dt->d0.a != 5.0f ) exit(89);
   if ( dt->d0.b != 10.0 ) exit(91);

}

void sub9a(const struct dt1 *dt) {

   if ( dt->a != 5.0f ) exit(93);
   if ( dt->b != 10.0 ) exit(95);
   if ( dt->d0.a != 5.0f ) exit(97);
   if ( dt->d0.b != 10.0 ) exit(99);

}

void sub10(struct dt1 dt) {

   if ( dt.a != 5.0f ) exit(101);
   if ( dt.b != 10.0 ) exit(103);
   if ( dt.d0.a != 5.0f ) exit(105);
   if ( dt.d0.b != 10.0 ) exit(107);

}

void sub10a(const struct dt1 dt) {

   if ( dt.a != 5.0f ) exit(109);
   if ( dt.b != 10.0 ) exit(111);
   if ( dt.d0.a != 5.0f ) exit(113);
   if ( dt.d0.b != 10.0 ) exit(115);

}

void sub11(struct dt2 *dt) {

   if ( dt->a != 5.0f ) exit(117);
   if ( dt->b != 10.0 ) exit(119);
   if ( dt->d1.a != 5.0f ) exit(121);
   if ( dt->d1.b != 10.0 ) exit(123);
   if ( dt->d1.d0.a != 5.0f ) exit(125);
   if ( dt->d1.d0.b != 10.0 ) exit(127);

}

void sub11a(const struct dt2 *dt) {

   if ( dt->a != 5.0f ) exit(129);
   if ( dt->b != 10.0 ) exit(131);
   if ( dt->d1.a != 5.0f ) exit(133);
   if ( dt->d1.b != 10.0 ) exit(135);
   if ( dt->d1.d0.a != 5.0f ) exit(137);
   if ( dt->d1.d0.b != 10.0 ) exit(141);

}

void sub12(struct dt2 dt) {

   if ( dt.a != 5.0f ) exit(143);
   if ( dt.b != 10.0 ) exit(145);
   if ( dt.d1.a != 5.0f ) exit(147);
   if ( dt.d1.b != 10.0 ) exit(149);
   if ( dt.d1.d0.a != 5.0f ) exit(151);
   if ( dt.d1.d0.b != 10.0 ) exit(153);

}

void sub12a(const struct dt2 dt) {

   if ( dt.a != 5.0f ) exit(155);
   if ( dt.b != 10.0 ) exit(157);
   if ( dt.d1.a != 5.0f ) exit(159);
   if ( dt.d1.b != 10.0 ) exit(161);
   if ( dt.d1.d0.a != 5.0f ) exit(163);
   if ( dt.d1.d0.b != 10.0 ) exit(165);

}

void sub13(struct dt0 *dt) {

   dt->a = dt->a + 5.0f;
   dt->b = dt->b + 10.0;

}

void sub14(struct dt1 *dt) {

   dt->a = dt->a + 5.0f;
   dt->b = dt->b + 10.0;
   dt->d0.a = dt->d0.a + 5.0f;
   dt->d0.b = dt->d0.b + 10.0;

}

void sub15(struct dt2 *dt) {

   dt->a = dt->a + 5.0f;
   dt->b = dt->b + 10.0;
   dt->d1.a = dt->d1.a + 5.0f;
   dt->d1.b = dt->d1.b + 10.0;
   dt->d1.d0.a = dt->d1.d0.a + 5.0f;
   dt->d1.d0.b = dt->d1.d0.b + 10.0;

}

