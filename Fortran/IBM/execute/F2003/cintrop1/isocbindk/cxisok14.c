/*
        C code for testcase "fxisok14.f" and "fxisok15.f"
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

char fnt1(struct dt0 *dt) {

   if ( dt->a != 'A' ) exit(21);
   if ( dt->b != 'B' ) exit(23);

   dt->a = 'C';
   dt->b = 'D';

   return 0;
}

char fnt2(struct dt0 dt) {

   if ( dt.a != 'A' ) exit(25);
   if ( dt.b != 'B' ) exit(27);

   dt.a = 'C';
   dt.b = 'D';

   return 0;
}

char fnt3(struct dt1 *dt) {

   if ( dt->a != 'A' ) exit(29);
   if ( dt->b != 'B' ) exit(31);
   if ( dt->d0.a != 'A' ) exit(33);
   if ( dt->d0.b != 'B' ) exit(35);

   dt->a = 'C';
   dt->b = 'D';
   dt->d0.a = 'C';
   dt->d0.b = 'D';

   return 0;
}

char fnt4(struct dt1 dt) {

   if ( dt.a != 'A' ) exit(37);
   if ( dt.b != 'B' ) exit(39);
   if ( dt.d0.a != 'A' ) exit(41);
   if ( dt.d0.b != 'B' ) exit(43);

   dt.a = 'C';
   dt.b = 'D';
   dt.d0.a = 'C';
   dt.d0.b = 'D';

   return 0;
}

char fnt5(struct dt2 *dt) {

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

   return 0;
}

char fnt6(struct dt2 dt) {

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

   return 0;
}

char fnt7(struct dt0 *dt) {

   if ( dt->a != 'A' ) exit(69);
   if ( dt->b != 'B' ) exit(71);

   return 0;
}

char fnt7a(const struct dt0 *dt) {

   if ( dt->a != 'A' ) exit(73);
   if ( dt->b != 'B' ) exit(75);

   return 0;
}

char fnt8(struct dt0 dt) {

   if ( dt.a != 'A' ) exit(77);
   if ( dt.b != 'B' ) exit(79);

   return 0;
}

char fnt8a(const struct dt0 dt) {

   if ( dt.a != 'A' ) exit(81);
   if ( dt.b != 'B' ) exit(83);

   return 0;
}

char fnt9(struct dt1 *dt) {

   if ( dt->a != 'A' ) exit(85);
   if ( dt->b != 'B' ) exit(87);
   if ( dt->d0.a != 'A' ) exit(89);
   if ( dt->d0.b != 'B' ) exit(91);

   return 0;
}

char fnt9a(const struct dt1 *dt) {

   if ( dt->a != 'A' ) exit(93);
   if ( dt->b != 'B' ) exit(95);
   if ( dt->d0.a != 'A' ) exit(97);
   if ( dt->d0.b != 'B' ) exit(99);

   return 0;
}

char fnt10(struct dt1 dt) {

   if ( dt.a != 'A' ) exit(101);
   if ( dt.b != 'B' ) exit(103);
   if ( dt.d0.a != 'A' ) exit(105);
   if ( dt.d0.b != 'B' ) exit(107);

   return 0;
}

char fnt10a(const struct dt1 dt) {

   if ( dt.a != 'A' ) exit(109);
   if ( dt.b != 'B' ) exit(111);
   if ( dt.d0.a != 'A' ) exit(113);
   if ( dt.d0.b != 'B' ) exit(115);

   return 0;
}

char fnt11(struct dt2 *dt) {

   if ( dt->a != 'A' ) exit(117);
   if ( dt->b != 'B' ) exit(119);
   if ( dt->d1.a != 'A' ) exit(121);
   if ( dt->d1.b != 'B' ) exit(123);
   if ( dt->d1.d0.a != 'A' ) exit(125);
   if ( dt->d1.d0.b != 'B' ) exit(127);

   return 0;
}

char fnt11a(const struct dt2 *dt) {

   if ( dt->a != 'A' ) exit(129);
   if ( dt->b != 'B' ) exit(131);
   if ( dt->d1.a != 'A' ) exit(133);
   if ( dt->d1.b != 'B' ) exit(135);
   if ( dt->d1.d0.a != 'A' ) exit(137);
   if ( dt->d1.d0.b != 'B' ) exit(141);

   return 0;
}

char fnt12(struct dt2 dt) {

   if ( dt.a != 'A' ) exit(143);
   if ( dt.b != 'B' ) exit(145);
   if ( dt.d1.a != 'A' ) exit(147);
   if ( dt.d1.b != 'B' ) exit(149);
   if ( dt.d1.d0.a != 'A' ) exit(151);
   if ( dt.d1.d0.b != 'B' ) exit(153);

   return 0;
}

char fnt12a(const struct dt2 dt) {

   if ( dt.a != 'A' ) exit(155);
   if ( dt.b != 'B' ) exit(157);
   if ( dt.d1.a != 'A' ) exit(159);
   if ( dt.d1.b != 'B' ) exit(161);
   if ( dt.d1.d0.a != 'A' ) exit(163);
   if ( dt.d1.d0.b != 'B' ) exit(165);

   return 0;
}

char fnt13(struct dt0 *dt) {

   dt->a = 'C';
   dt->b = 'D';

   return 0;
}

char fnt14(struct dt1 *dt) {

   dt->a = 'C';
   dt->b = 'D';
   dt->d0.a = 'C';
   dt->d0.b = 'D';

   return 0;
}

char fnt15(struct dt2 *dt) {

   dt->a = 'C';
   dt->b = 'D';
   dt->d1.a = 'C';
   dt->d1.b = 'D';
   dt->d1.d0.a = 'C';
   dt->d1.d0.b = 'D';

   return 0;
}

