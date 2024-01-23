/*
        C code for testcase "fxisoh20a.f" and "fxisoh21a.f"
*/

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

struct dts0 {
   int_fast32_t a[5];
   int_fast64_t b[3];
};

struct dts1 {
   int_fast32_t a[5];
   int_fast64_t b[3];
   struct dts0 d0;
};

struct dts2 {
   int_fast32_t a[5];
   int_fast64_t b[3];
   struct dts1 d1;
};

int fnt1(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != i+1 ) exit(21);
      dt->a[i] = i+2;
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != i+1 ) exit(23);
      dt->b[i] = i+2;
   }

   return 0;
}

int fnt2(struct dts0 dt) {
   int i; 


   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != i+1 ) exit(25);
      dt.a[i] = i+2;
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != i+1 ) exit(27);
      dt.b[i] = i+2;
   }

   return 0;
}

int fnt3(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != i+1 ) exit(29);
      dt->a[i] = dt->a[i]+i+1;
      if ( dt->d0.a[i] != i+1 ) exit(31);
      dt->d0.a[i] = dt->d0.a[i]+i+1;
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != i+1 ) exit(33);
      dt->b[i] = dt->b[i]+i+1;
      if ( dt->d0.b[i] != i+1 ) exit(35);
      dt->d0.b[i] = dt->d0.b[i]+i+1;
   }

   return 0;
}

int fnt4(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != i+1 ) exit(37);
      dt.a[i] = dt.a[i]+i+1;
      if ( dt.d0.a[i] != i+1 ) exit(39);
      dt.d0.a[i] = dt.d0.a[i]+i+1;
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != i+1 ) exit(41);
      dt.b[i] = dt.b[i]+i+1;
      if ( dt.d0.b[i] != i+1 ) exit(43);
      dt.d0.b[i] = dt.d0.b[i]+i+1;
   }

   return 0;
}

int fnt5(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != i+1 ) exit(45);
      dt->a[i] = dt->a[i]+i+1;
      if ( dt->d1.a[i] != i+1 ) exit(47);
      dt->d1.a[i] = dt->d1.a[i]+i+1;
      if ( dt->d1.d0.a[i] != i+1 ) exit(49);
      dt->d1.d0.a[i] = dt->d1.d0.a[i]+i+1;
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != i+1 ) exit(51);
      dt->b[i] = dt->b[i]+i+1;
      if ( dt->d1.b[i] != i+1 ) exit(53);
      dt->d1.b[i] = dt->d1.b[i]+i+1;
      if ( dt->d1.d0.b[i] != i+1 ) exit(55);
      dt->d1.d0.b[i] = dt->d1.d0.b[i]+i+1;
   }

   return 0;
}

int fnt6(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != i+1 ) exit(57);
      dt.a[i] = dt.a[i]+i+1;
      if ( dt.d1.a[i] != i+1 ) exit(59);
      dt.d1.a[i] = dt.d1.a[i]+i+1;
      if ( dt.d1.d0.a[i] != i+1 ) exit(61);
      dt.d1.d0.a[i] = dt.d1.d0.a[i]+i+1;
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != i+1 ) exit(63);
      dt.b[i] = dt.b[i]+i+1;
      if ( dt.d1.b[i] != i+1 ) exit(65);
      dt.d1.b[i] = dt.d1.b[i]+i+1;
      if ( dt.d1.d0.b[i] != i+1 ) exit(67);
      dt.d1.d0.b[i] = dt.d1.d0.b[i]+i+1;
   }

   return 0;
}

int fnt7(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != i+1 ) exit(69);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != i+1 ) exit(71);
   }

   return 0;
}

int fnt7a(const struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != i+1 ) exit(73);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != i+1 ) exit(75);
   }

   return 0;
}

int fnt8(struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != i+1 ) exit(77);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != i+1 ) exit(79);
   }

   return 0;
}

int fnt8a(const struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != i+1 ) exit(81);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != i+1 ) exit(83);
   }

   return 0;
}

int fnt9(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != i+1 ) exit(85);
      if ( dt->d0.a[i] != i+1 ) exit(87);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != i+1 ) exit(89);
      if ( dt->d0.b[i] != i+1 ) exit(91);
   }

   return 0;
}

int fnt9a(const struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != i+1 ) exit(93);
      if ( dt->d0.a[i] != i+1 ) exit(95);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != i+1 ) exit(97);
      if ( dt->d0.b[i] != i+1 ) exit(99);
   }

   return 0;
}

int fnt10(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != i+1 ) exit(101);
      if ( dt.d0.a[i] != i+1 ) exit(103);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != i+1 ) exit(105);
      if ( dt.d0.b[i] != i+1 ) exit(107);
   }

   return 0;
}

int fnt10a(const struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != i+1 ) exit(109);
      if ( dt.d0.a[i] != i+1 ) exit(111);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != i+1 ) exit(113);
      if ( dt.d0.b[i] != i+1 ) exit(115);
   }

   return 0;
}

int fnt11(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != i+1 ) exit(117);
      if ( dt->d1.a[i] != i+1 ) exit(119);
      if ( dt->d1.d0.a[i] != i+1 ) exit(121);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != i+1 ) exit(123);
      if ( dt->d1.b[i] != i+1 ) exit(125);
      if ( dt->d1.d0.b[i] != i+1 ) exit(127);
   }

   return 0;
}

int fnt11a(const struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != i+1 ) exit(129);
      if ( dt->d1.a[i] != i+1 ) exit(131);
      if ( dt->d1.d0.a[i] != i+1 ) exit(133);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt->b[i] != i+1 ) exit(135);
      if ( dt->d1.b[i] != i+1 ) exit(137);
      if ( dt->d1.d0.b[i] != i+1 ) exit(141);
   }

   return 0;
}

int fnt12(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != i+1 ) exit(143);
      if ( dt.d1.a[i] != i+1 ) exit(145);
      if ( dt.d1.d0.a[i] != i+1 ) exit(147);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != i+1 ) exit(149);
      if ( dt.d1.b[i] != i+1 ) exit(151);
      if ( dt.d1.d0.b[i] != i+1 ) exit(153);
   }

   return 0;
}

int fnt12a(const struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != i+1 ) exit(155);
      if ( dt.d1.a[i] != i+1 ) exit(157);
      if ( dt.d1.d0.a[i] != i+1 ) exit(159);
   }

   for ( i = 0; i < 3; i++ ) {
      if ( dt.b[i] != i+1 ) exit(161);
      if ( dt.d1.b[i] != i+1 ) exit(163);
      if ( dt.d1.d0.b[i] != i+1 ) exit(165);
   }

   return 0;
}

int fnt13(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i]+i+1;
   }

   for ( i = 0; i < 3; i++ ) {
      dt->b[i] = dt->b[i]+i+1;
   }

   return 0;
}

int fnt14(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i]+i+1;
      dt->d0.a[i] = dt->d0.a[i]+i+1;
   }

   for ( i = 0; i < 3; i++ ) {
      dt->b[i] = dt->b[i]+i+1;
      dt->d0.b[i] = dt->d0.b[i]+i+1;
   }

   return 0;
}

int fnt15(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i]+i+1;
      dt->d1.a[i] = dt->d1.a[i]+i+1;
      dt->d1.d0.a[i] = dt->d1.d0.a[i]+i+1;
   }

   for ( i = 0; i < 3; i++ ) {
      dt->b[i] = dt->b[i]+i+1;
      dt->d1.b[i] = dt->d1.b[i]+i+1;
      dt->d1.d0.b[i] = dt->d1.d0.b[i]+i+1;
   }

   return 0;
}
