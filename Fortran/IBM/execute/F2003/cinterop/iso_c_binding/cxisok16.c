/*
        C code for testcase "fxisok16.f" and "fxisok17.f"
*/

#include <stdio.h>
#include <stdlib.h>

struct dt0 {
   char a;
   signed char b;
};

struct dt1 {
   char a;
   signed char b;
   struct dt0 d0;
};

struct dt2 {
   char a;
   signed char b;
   struct dt1 d1;
};

void sub1(struct dt0 *dt) {

   if ( dt->a != 'A' ) exit(21);
   if ( dt->b != 'B' ) exit(23);

   dt->a = 'C';
   dt->b = 'D';

}

void sub2(struct dt0 dt) {

   if ( dt.a != 'A' ) exit(25);
   if ( dt.b != 'B' ) exit(27);

   dt.a = 'C';
   dt.b = 'D';

}

void sub3(struct dt1 *dt) {

   if ( dt->a != 'A' ) exit(29);
   if ( dt->b != 'B' ) exit(31);
   if ( dt->d0.a != 'A' ) exit(33);
   if ( dt->d0.b != 'B' ) exit(35);

   dt->a = 'C';
   dt->b = 'D';
   dt->d0.a = 'C';
   dt->d0.b = 'D';

}

void sub4(struct dt1 dt) {

   if ( dt.a != 'A' ) exit(37);
   if ( dt.b != 'B' ) exit(39);
   if ( dt.d0.a != 'A' ) exit(41);
   if ( dt.d0.b != 'B' ) exit(43);

   dt.a = 'C';
   dt.b = 'D';
   dt.d0.a = 'C';
   dt.d0.b = 'D';

}

void sub5(struct dt2 *dt) {

   if ( dt->a != 'A' ) exit(45);
   if ( dt->b != 'B' ) exit(47);
   if ( dt->d1.a != 'A' ) exit(49);
   if ( dt->d1.b != 'B' ) exit(51);
   if ( dt->d1.d0.a != 'A' ) exit(53);
   if ( dt->d1.d0.b != 'B' ) exit(55);

   dt->a = 'C';
   dt->b = 'D';
   dt->d1.a = 'C';
   dt->d1.b = 'D';
   dt->d1.d0.a = 'C';
   dt->d1.d0.b = 'D';

}

void sub6(struct dt2 dt) {

   if ( dt.a != 'A' ) exit(57);
   if ( dt.b != 'B' ) exit(59);
   if ( dt.d1.a != 'A' ) exit(61);
   if ( dt.d1.b != 'B' ) exit(63);
   if ( dt.d1.d0.a != 'A' ) exit(65);
   if ( dt.d1.d0.b != 'B' ) exit(67);

   dt.a = 'C';
   dt.b = 'D';
   dt.d1.a = 'C';
   dt.d1.b = 'D';
   dt.d1.d0.a = 'C';
   dt.d1.d0.b = 'D';

}

void sub7(struct dt0 *dt) {

   if ( dt->a != 'A' ) exit(69);
   if ( dt->b != 'B' ) exit(71);

}

void sub7a(const struct dt0 *dt) {

   if ( dt->a != 'A' ) exit(73);
   if ( dt->b != 'B' ) exit(75);

}

void sub8(struct dt0 dt) {

   if ( dt.a != 'A' ) exit(77);
   if ( dt.b != 'B' ) exit(79);

}

void sub8a(const struct dt0 dt) {

   if ( dt.a != 'A' ) exit(81);
   if ( dt.b != 'B' ) exit(83);

}

void sub9(struct dt1 *dt) {

   if ( dt->a != 'A' ) exit(85);
   if ( dt->b != 'B' ) exit(87);
   if ( dt->d0.a != 'A' ) exit(89);
   if ( dt->d0.b != 'B' ) exit(91);

}

void sub9a(const struct dt1 *dt) {

   if ( dt->a != 'A' ) exit(93);
   if ( dt->b != 'B' ) exit(95);
   if ( dt->d0.a != 'A' ) exit(97);
   if ( dt->d0.b != 'B' ) exit(99);

}

void sub10(struct dt1 dt) {

   if ( dt.a != 'A' ) exit(101);
   if ( dt.b != 'B' ) exit(103);
   if ( dt.d0.a != 'A' ) exit(105);
   if ( dt.d0.b != 'B' ) exit(107);

}

void sub10a(const struct dt1 dt) {

   if ( dt.a != 'A' ) exit(109);
   if ( dt.b != 'B' ) exit(111);
   if ( dt.d0.a != 'A' ) exit(113);
   if ( dt.d0.b != 'B' ) exit(115);

}

void sub11(struct dt2 *dt) {

   if ( dt->a != 'A' ) exit(117);
   if ( dt->b != 'B' ) exit(119);
   if ( dt->d1.a != 'A' ) exit(121);
   if ( dt->d1.b != 'B' ) exit(123);
   if ( dt->d1.d0.a != 'A' ) exit(125);
   if ( dt->d1.d0.b != 'B' ) exit(127);

}

void sub11a(const struct dt2 *dt) {

   if ( dt->a != 'A' ) exit(129);
   if ( dt->b != 'B' ) exit(131);
   if ( dt->d1.a != 'A' ) exit(133);
   if ( dt->d1.b != 'B' ) exit(135);
   if ( dt->d1.d0.a != 'A' ) exit(137);
   if ( dt->d1.d0.b != 'B' ) exit(141);

}

void sub12(struct dt2 dt) {

   if ( dt.a != 'A' ) exit(143);
   if ( dt.b != 'B' ) exit(145);
   if ( dt.d1.a != 'A' ) exit(147);
   if ( dt.d1.b != 'B' ) exit(149);
   if ( dt.d1.d0.a != 'A' ) exit(151);
   if ( dt.d1.d0.b != 'B' ) exit(153);

}

void sub12a(const struct dt2 dt) {

   if ( dt.a != 'A' ) exit(155);
   if ( dt.b != 'B' ) exit(157);
   if ( dt.d1.a != 'A' ) exit(159);
   if ( dt.d1.b != 'B' ) exit(161);
   if ( dt.d1.d0.a != 'A' ) exit(163);
   if ( dt.d1.d0.b != 'B' ) exit(165);

}

void sub13(struct dt0 *dt) {

   dt->a = 'C';
   dt->b = 'D';

}

void sub14(struct dt1 *dt) {

   dt->a = 'C';
   dt->b = 'D';
   dt->d0.a = 'C';
   dt->d0.b = 'D';

}

void sub15(struct dt2 *dt) {

   dt->a = 'C';
   dt->b = 'D';
   dt->d1.a = 'C';
   dt->d1.b = 'D';
   dt->d1.d0.a = 'C';
   dt->d1.d0.b = 'D';

}

