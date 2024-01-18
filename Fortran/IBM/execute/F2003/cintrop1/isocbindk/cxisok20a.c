/*
        C code for testcase "fxisok20a.f" and "fxisok21a.f"
*/

#include <stdio.h>
#include <stdlib.h>

struct dts0 {
   char a[4];
   signed char b[3];
};

struct dts1 {
   char a[4];
   signed char b[3];
   struct dts0 d0;
};

struct dts2 {
   char a[4];
   signed char b[3];
   struct dts1 d1;
};

int fnt1(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt->a[i] != 'A'+i ) exit(21);
      dt->a[i] = 'A'+i+4;
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != 'A'+i ) exit(23);
      dt->b[i] = 'A'+i+4;
   }

   return 0;
}

int fnt2(struct dts0 dt) {
   int i; 


   for ( i = 0; i < 4; i++ ) {
      if ( dt.a[i] != 'A'+i ) exit(25);
      dt.a[i] = 'A'+i+4;
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != 'A'+i ) exit(27);
      dt.b[i] = 'A'+i+4;
   }

   return 0;
}

int fnt3(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt->a[i] != 'A'+i ) exit(29);
      dt->a[i] = 'A'+i+4;
      if ( dt->d0.a[i] != 'A'+i ) exit(31);
      dt->d0.a[i] = 'A'+i+4;
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != 'A'+i ) exit(33);
      dt->b[i] = 'A'+i+4;
      if ( dt->d0.b[i] != 'A'+i ) exit(35);
      dt->d0.b[i] = 'A'+i+4;
   }

   return 0;
}

int fnt4(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt.a[i] != 'A'+i ) exit(37);
      dt.a[i] = 'A'+i+4;
      if ( dt.d0.a[i] != 'A'+i ) exit(39);
      dt.d0.a[i] = 'A'+i+4;
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != 'A'+i ) exit(41);
      dt.b[i] = 'A'+i+4;
      if ( dt.d0.b[i] != 'A'+i ) exit(43);
      dt.d0.b[i] = 'A'+i+4;
   }

   return 0;
}

int fnt5(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt->a[i] != 'A'+i ) exit(45);
      dt->a[i] = 'A'+i+4;
      if ( dt->d1.a[i] != 'A'+i ) exit(47);
      dt->d1.a[i] = 'A'+i+4;
      if ( dt->d1.d0.a[i] != 'A'+i ) exit(49);
      dt->d1.d0.a[i] = 'A'+i+4;
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != 'A'+i ) exit(51);
      dt->b[i] = 'A'+i+4;
      if ( dt->d1.b[i] != 'A'+i ) exit(53);
      dt->d1.b[i] = 'A'+i+4;
      if ( dt->d1.d0.b[i] != 'A'+i ) exit(55);
      dt->d1.d0.b[i] = 'A'+i+4;
   }

   return 0;
}

int fnt6(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt.a[i] != 'A'+i ) exit(57);
      dt.a[i] = 'A'+i+4;
      if ( dt.d1.a[i] != 'A'+i ) exit(59);
      dt.d1.a[i] = 'A'+i+4;
      if ( dt.d1.d0.a[i] != 'A'+i ) exit(61);
      dt.d1.d0.a[i] = 'A'+i+4;
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != 'A'+i ) exit(63);
      dt.b[i] = 'A'+i+4;
      if ( dt.d1.b[i] != 'A'+i ) exit(65);
      dt.d1.b[i] = 'A'+i+4;
      if ( dt.d1.d0.b[i] != 'A'+i ) exit(67);
      dt.d1.d0.b[i] = 'A'+i+4;
   }

   return 0;
}

int fnt7(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt->a[i] != 'A'+i ) exit(69);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != 'A'+i ) exit(71);
   }

   return 0;
}

int fnt7a(const struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt->a[i] != 'A'+i ) exit(73);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != 'A'+i ) exit(75);
   }

   return 0;
}

int fnt8(struct dts0 dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt.a[i] != 'A'+i ) exit(77);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != 'A'+i ) exit(79);
   }

   return 0;
}

int fnt8a(const struct dts0 dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt.a[i] != 'A'+i ) exit(81);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != 'A'+i ) exit(83);
   }

   return 0;
}

int fnt9(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt->a[i] != 'A'+i ) exit(85);
      if ( dt->d0.a[i] != 'A'+i ) exit(87);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != 'A'+i ) exit(89);
      if ( dt->d0.b[i] != 'A'+i ) exit(91);
   }

   return 0;
}

int fnt9a(const struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt->a[i] != 'A'+i ) exit(93);
      if ( dt->d0.a[i] != 'A'+i ) exit(95);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != 'A'+i ) exit(97);
      if ( dt->d0.b[i] != 'A'+i ) exit(99);
   }

   return 0;
}

int fnt10(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt.a[i] != 'A'+i ) exit(101);
      if ( dt.d0.a[i] != 'A'+i ) exit(103);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != 'A'+i ) exit(105);
      if ( dt.d0.b[i] != 'A'+i ) exit(107);
   }

   return 0;
}

int fnt10a(const struct dts1 dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt.a[i] != 'A'+i ) exit(109);
      if ( dt.d0.a[i] != 'A'+i ) exit(111);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != 'A'+i ) exit(113);
      if ( dt.d0.b[i] != 'A'+i ) exit(115);
   }

   return 0;
}

int fnt11(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt->a[i] != 'A'+i ) exit(117);
      if ( dt->d1.a[i] != 'A'+i ) exit(119);
      if ( dt->d1.d0.a[i] != 'A'+i ) exit(121);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != 'A'+i ) exit(123);
      if ( dt->d1.b[i] != 'A'+i ) exit(125);
      if ( dt->d1.d0.b[i] != 'A'+i ) exit(127);
   }

   return 0;
}

int fnt11a(const struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt->a[i] != 'A'+i ) exit(129);
      if ( dt->d1.a[i] != 'A'+i ) exit(131);
      if ( dt->d1.d0.a[i] != 'A'+i ) exit(133);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != 'A'+i ) exit(135);
      if ( dt->d1.b[i] != 'A'+i ) exit(137);
      if ( dt->d1.d0.b[i] != 'A'+i ) exit(141);
   }

   return 0;
}

int fnt12(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt.a[i] != 'A'+i ) exit(143);
      if ( dt.d1.a[i] != 'A'+i ) exit(145);
      if ( dt.d1.d0.a[i] != 'A'+i ) exit(147);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != 'A'+i ) exit(149);
      if ( dt.d1.b[i] != 'A'+i ) exit(151);
      if ( dt.d1.d0.b[i] != 'A'+i ) exit(153);
   }

   return 0;
}

int fnt12a(const struct dts2 dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      if ( dt.a[i] != 'A'+i ) exit(155);
      if ( dt.d1.a[i] != 'A'+i ) exit(157);
      if ( dt.d1.d0.a[i] != 'A'+i ) exit(159);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != 'A'+i ) exit(161);
      if ( dt.d1.b[i] != 'A'+i ) exit(163);
      if ( dt.d1.d0.b[i] != 'A'+i ) exit(165);
   }

   return 0;
}

int fnt13(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      dt->a[i] = 'A'+i+4;
   }

   for ( i = 0; i < 3; i++ ) {
      dt->b[i] = 'A'+i+4;
   }

   return 0;
}

int fnt14(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      dt->a[i] = 'A'+i+4;
      dt->d0.a[i] = 'A'+i+4;
   }

   for ( i = 0; i < 3; i++ ) {
      dt->b[i] = 'A'+i+4;
      dt->d0.b[i] = 'A'+i+4;
   }

   return 0;
}

int fnt15(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 4; i++ ) {
      dt->a[i] = 'A'+i+4;
      dt->d1.a[i] = 'A'+i+4;
      dt->d1.d0.a[i] = 'A'+i+4;
   }

   for ( i = 0; i < 3; i++ ) {
      dt->b[i] = 'A'+i+4;
      dt->d1.b[i] = 'A'+i+4;
      dt->d1.d0.b[i] = 'A'+i+4;
   }

   return 0;
}
