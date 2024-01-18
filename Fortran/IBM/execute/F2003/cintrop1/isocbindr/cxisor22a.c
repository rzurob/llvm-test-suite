/*
        C code for testcase "fxisor22a.f" and "fxisor23a.f"
*/

#include <stdio.h>
#include <stdlib.h>


struct dts0 {
   _Bool a[5];
};

struct dts1 {
   _Bool a[5];
   struct dts0 d0;
};

struct dts2 {
   _Bool a[5];
   struct dts1 d1;
};

void sub1(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != 1 ) exit(21);
      dt->a[i] = 0;
   }


}

void sub2(struct dts0 dt) {
   int i; 


   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != 1 ) exit(23);
      dt.a[i] = 0;
   }


}

void sub3(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != 1 ) exit(25);
      dt->a[i] = 0;
      if ( dt->d0.a[i] != 1 ) exit(27);
      dt->d0.a[i] = 0;
   }


}

void sub4(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != 1 ) exit(29);
      dt.a[i] = 0;
      if ( dt.d0.a[i] != 1 ) exit(31);
      dt.d0.a[i] = 0;
   }


}

void sub5(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != 1 ) exit(33);
      dt->a[i] = 0;
      if ( dt->d1.a[i] != 1 ) exit(35);
      dt->d1.a[i] = 0;
      if ( dt->d1.d0.a[i] != 1 ) exit(37);
      dt->d1.d0.a[i] = 0;
   }


}

void sub6(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != 1 ) exit(39);
      dt.a[i] = 0;
      if ( dt.d1.a[i] != 1 ) exit(41);
      dt.d1.a[i] = 0;
      if ( dt.d1.d0.a[i] != 1 ) exit(43);
      dt.d1.d0.a[i] = 0;
   }


}

void sub7(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != 1 ) exit(45);
   }


}

void sub7a(const struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != 1 ) exit(47);
   }


}

void sub8(struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != 1 ) exit(49);
   }


}

void sub8a(const struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != 1 ) exit(51);
   }


}

void sub9(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != 1 ) exit(53);
      if ( dt->d0.a[i] != 1 ) exit(55);
   }


}

void sub9a(const struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != 1 ) exit(57);
      if ( dt->d0.a[i] != 1 ) exit(59);
   }


}

void sub10(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != 1 ) exit(61);
      if ( dt.d0.a[i] != 1 ) exit(63);
   }


}

void sub10a(const struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != 1 ) exit(65);
      if ( dt.d0.a[i] != 1 ) exit(67);
   }


}

void sub11(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != 1 ) exit(69);
      if ( dt->d1.a[i] != 1 ) exit(71);
      if ( dt->d1.d0.a[i] != 1 ) exit(73);
   }


}

void sub11a(const struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != 1 ) exit(75);
      if ( dt->d1.a[i] != 1 ) exit(77);
      if ( dt->d1.d0.a[i] != 1 ) exit(79);
   }


}

void sub12(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != 1 ) exit(81);
      if ( dt.d1.a[i] != 1 ) exit(83);
      if ( dt.d1.d0.a[i] != 1 ) exit(85);
   }


}

void sub12a(const struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != 1 ) exit(87);
      if ( dt.d1.a[i] != 1 ) exit(89);
      if ( dt.d1.d0.a[i] != 1 ) exit(91);
   }


}

void sub13(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = 0;
   }


}

void sub14(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = 0;
      dt->d0.a[i] = 0;
   }


}

void sub15(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = 0;
      dt->d1.a[i] = 0;
      dt->d1.d0.a[i] = 0;
   }


}
