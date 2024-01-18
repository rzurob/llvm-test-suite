/*
        C code for testcase "fxisoh16.f" and "fxisoh17.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

struct dt0 {
   int_fast32_t a;
   int_fast64_t b;
};

struct dt1 {
   int_fast32_t a;
   int_fast64_t b;
   struct dt0 d0;
};

struct dt2 {
   int_fast32_t a;
   int_fast64_t b;
   struct dt1 d1;
};

void sub1(struct dt0 *dt) {

   if ( dt->a != 5 ) exit(21);
   if ( dt->b != 10 ) exit(23);

   dt->a = dt->a + 5;
   dt->b = dt->b + 10;

}

void sub2(struct dt0 dt) {

   if ( dt.a != 5 ) exit(25);
   if ( dt.b != 10 ) exit(27);

   dt.a = dt.a + 5;
   dt.b = dt.b + 10;

}

void sub3(struct dt1 *dt) {

   if ( dt->a != 5 ) exit(29);
   if ( dt->b != 10 ) exit(31);
   if ( dt->d0.a != 5 ) exit(33);
   if ( dt->d0.b != 10 ) exit(35);

   dt->a = dt->a + 5;
   dt->b = dt->b + 10;
   dt->d0.a = dt->d0.a + 5;
   dt->d0.b = dt->d0.b + 10;

}

void sub4(struct dt1 dt) {

   if ( dt.a != 5 ) exit(37);
   if ( dt.b != 10 ) exit(39);
   if ( dt.d0.a != 5 ) exit(41);
   if ( dt.d0.b != 10 ) exit(43);

   dt.a = dt.a + 5;
   dt.b = dt.b + 10;
   dt.d0.a = dt.d0.a + 5;
   dt.d0.b = dt.d0.b + 10;

}

void sub5(struct dt2 *dt) {

   if ( dt->a != 5 ) exit(45);
   if ( dt->b != 10 ) exit(47);
   if ( dt->d1.a != 5 ) exit(49);
   if ( dt->d1.b != 10 ) exit(51);
   if ( dt->d1.d0.a != 5 ) exit(53);
   if ( dt->d1.d0.b != 10 ) exit(55);

   dt->a = dt->a + 5;
   dt->b = dt->b + 10;
   dt->d1.a = dt->d1.a + 5;
   dt->d1.b = dt->d1.b + 10;
   dt->d1.d0.a = dt->d1.d0.a + 5;
   dt->d1.d0.b = dt->d1.d0.b + 10;

}

void sub6(struct dt2 dt) {

   if ( dt.a != 5 ) exit(57);
   if ( dt.b != 10 ) exit(59);
   if ( dt.d1.a != 5 ) exit(61);
   if ( dt.d1.b != 10 ) exit(63);
   if ( dt.d1.d0.a != 5 ) exit(65);
   if ( dt.d1.d0.b != 10 ) exit(67);

   dt.a = dt.a + 5;
   dt.b = dt.b + 10;
   dt.d1.a = dt.d1.a + 5;
   dt.d1.b = dt.d1.b + 10;
   dt.d1.d0.a = dt.d1.d0.a + 5;
   dt.d1.d0.b = dt.d1.d0.b + 10;

}

void sub7(struct dt0 *dt) {

   if ( dt->a != 5 ) exit(69);
   if ( dt->b != 10 ) exit(71);

}

void sub7a(const struct dt0 *dt) {

   if ( dt->a != 5 ) exit(73);
   if ( dt->b != 10 ) exit(75);

}

void sub8(struct dt0 dt) {

   if ( dt.a != 5 ) exit(77);
   if ( dt.b != 10 ) exit(79);

}

void sub8a(const struct dt0 dt) {

   if ( dt.a != 5 ) exit(81);
   if ( dt.b != 10 ) exit(83);

}

void sub9(struct dt1 *dt) {

   if ( dt->a != 5 ) exit(85);
   if ( dt->b != 10 ) exit(87);
   if ( dt->d0.a != 5 ) exit(89);
   if ( dt->d0.b != 10 ) exit(91);

}

void sub9a(const struct dt1 *dt) {

   if ( dt->a != 5 ) exit(93);
   if ( dt->b != 10 ) exit(95);
   if ( dt->d0.a != 5 ) exit(97);
   if ( dt->d0.b != 10 ) exit(99);

}

void sub10(struct dt1 dt) {

   if ( dt.a != 5 ) exit(101);
   if ( dt.b != 10 ) exit(103);
   if ( dt.d0.a != 5 ) exit(105);
   if ( dt.d0.b != 10 ) exit(107);

}

void sub10a(const struct dt1 dt) {

   if ( dt.a != 5 ) exit(109);
   if ( dt.b != 10 ) exit(111);
   if ( dt.d0.a != 5 ) exit(113);
   if ( dt.d0.b != 10 ) exit(115);

}

void sub11(struct dt2 *dt) {

   if ( dt->a != 5 ) exit(117);
   if ( dt->b != 10 ) exit(119);
   if ( dt->d1.a != 5 ) exit(121);
   if ( dt->d1.b != 10 ) exit(123);
   if ( dt->d1.d0.a != 5 ) exit(125);
   if ( dt->d1.d0.b != 10 ) exit(127);

}

void sub11a(const struct dt2 *dt) {

   if ( dt->a != 5 ) exit(129);
   if ( dt->b != 10 ) exit(131);
   if ( dt->d1.a != 5 ) exit(133);
   if ( dt->d1.b != 10 ) exit(135);
   if ( dt->d1.d0.a != 5 ) exit(137);
   if ( dt->d1.d0.b != 10 ) exit(141);

}

void sub12(struct dt2 dt) {

   if ( dt.a != 5 ) exit(143);
   if ( dt.b != 10 ) exit(145);
   if ( dt.d1.a != 5 ) exit(147);
   if ( dt.d1.b != 10 ) exit(149);
   if ( dt.d1.d0.a != 5 ) exit(151);
   if ( dt.d1.d0.b != 10 ) exit(153);

}

void sub12a(const struct dt2 dt) {

   if ( dt.a != 5 ) exit(155);
   if ( dt.b != 10 ) exit(157);
   if ( dt.d1.a != 5 ) exit(159);
   if ( dt.d1.b != 10 ) exit(161);
   if ( dt.d1.d0.a != 5 ) exit(163);
   if ( dt.d1.d0.b != 10 ) exit(165);

}

void sub13(struct dt0 *dt) {

   dt->a = dt->a + 5;
   dt->b = dt->b + 10;

}

void sub14(struct dt1 *dt) {

   dt->a = dt->a + 5;
   dt->b = dt->b + 10;
   dt->d0.a = dt->d0.a + 5;
   dt->d0.b = dt->d0.b + 10;

}

void sub15(struct dt2 *dt) {

   dt->a = dt->a + 5;
   dt->b = dt->b + 10;
   dt->d1.a = dt->d1.a + 5;
   dt->d1.b = dt->d1.b + 10;
   dt->d1.d0.a = dt->d1.d0.a + 5;
   dt->d1.d0.b = dt->d1.d0.b + 10;

}

