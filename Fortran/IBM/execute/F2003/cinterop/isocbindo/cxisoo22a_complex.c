/*
        C code for testcase "fxisoo22a.f" and "fxisoo23a.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

struct dts0 {
   long double _Complex a[5];
};

struct dts1 {
   long double _Complex a[5];
   struct dts0 d0;
};

struct dts2 {
   long double _Complex a[5];
   struct dts1 d1;
};

void sub1(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(21);
      dt->a[i] = (long double)(i+2)+I*(long double)(i+2);
   }


}

void sub2(struct dts0 dt) {
   int i; 


   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(25);
      dt.a[i] = (long double)(i+2)+I*(long double)(i+2);
   }


}

void sub3(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(29);
      dt->a[i] = dt->a[i] + (long double)(i+1)+I*(long double)(i+1);
      if ( dt->d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(33);
      dt->d0.a[i] = dt->d0.a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


}

void sub4(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(37);
      dt.a[i] = dt.a[i] + (long double)(i+1)+I*(long double)(i+1);
      if ( dt.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(41);
      dt.d0.a[i] = dt.d0.a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


}

void sub5(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(45);
      dt->a[i] = dt->a[i] + (long double)(i+1)+I*(long double)(i+1);
      if ( dt->d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(49);
      dt->d1.a[i] = dt->d1.a[i] + (long double)(i+1)+I*(long double)(i+1);
      if ( dt->d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(53);
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


}

void sub6(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(57);
      dt.a[i] = dt.a[i] + (long double)(i+1)+I*(long double)(i+1);
      if ( dt.d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(61);
      dt.d1.a[i] = dt.d1.a[i] + (long double)(i+1)+I*(long double)(i+1);
      if ( dt.d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(65);
      dt.d1.d0.a[i] = dt.d1.d0.a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


}

void sub7(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(69);
   }


}

void sub7a(const struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(73);
   }


}

void sub8(struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(77);
   }


}

void sub8a(const struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(81);
   }


}

void sub9(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(85);
      if ( dt->d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(89);
   }


}

void sub9a(const struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(93);
      if ( dt->d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(97);
   }


}

void sub10(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(101);
      if ( dt.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(105);
   }


}

void sub10a(const struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(109);
      if ( dt.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(113);
   }


}

void sub11(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(117);
      if ( dt->d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(121);
      if ( dt->d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(125);
   }


}

void sub11a(const struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(129);
      if ( dt->d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(133);
      if ( dt->d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(137);
   }


}

void sub12(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(143);
      if ( dt.d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(147);
      if ( dt.d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(151);
   }


}

void sub12a(const struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(155);
      if ( dt.d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(159);
      if ( dt.d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(163);
   }


}

void sub13(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


}

void sub14(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + (long double)(i+1)+I*(long double)(i+1);
      dt->d0.a[i] = dt->d0.a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


}

void sub15(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + (long double)(i+1)+I*(long double)(i+1);
      dt->d1.a[i] = dt->d1.a[i] + (long double)(i+1)+I*(long double)(i+1);
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


}
