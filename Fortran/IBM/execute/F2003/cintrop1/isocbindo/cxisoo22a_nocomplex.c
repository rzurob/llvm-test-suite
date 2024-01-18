/*
        C code for testcase "fxisoo22a.f" and "fxisoo23a.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

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
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(23);
      dt->a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
   }


}

void sub2(struct dts0 dt) {
   int i; 


   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(27);
      dt.a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
   }


}

void sub3(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(31);
      dt->a[i] = dt->a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      if ( dt->d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(35);
      dt->d0.a[i] = dt->d0.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


}

void sub4(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(39);
      dt.a[i] = dt.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      if ( dt.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(43);
      dt.d0.a[i] = dt.d0.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


}

void sub5(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(47);
      dt->a[i] = dt->a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      if ( dt->d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(51);
      dt->d1.a[i] = dt->d1.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      if ( dt->d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(55);
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


}

void sub6(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(59);
      dt.a[i] = dt.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      if ( dt.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(63);
      dt.d1.a[i] = dt.d1.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      if ( dt.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(67);
      dt.d1.d0.a[i] = dt.d1.d0.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


}

void sub7(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(71);
   }


}

void sub7a(const struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(75);
   }


}

void sub8(struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(79);
   }


}

void sub8a(const struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(83);
   }


}

void sub9(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(87);
      if ( dt->d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(91);
   }


}

void sub9a(const struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(95);
      if ( dt->d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(99);
   }


}

void sub10(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(103);
      if ( dt.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(107);
   }


}

void sub10a(const struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(111);
      if ( dt.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(115);
   }


}

void sub11(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(119);
      if ( dt->d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(123);
      if ( dt->d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(127);
   }


}

void sub11a(const struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(131);
      if ( dt->d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(135);
      if ( dt->d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(141);
   }


}

void sub12(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(145);
      if ( dt.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(149);
      if ( dt.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(153);
   }


}

void sub12a(const struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(157);
      if ( dt.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(161);
      if ( dt.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(165);
   }


}

void sub13(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


}

void sub14(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      dt->d0.a[i] = dt->d0.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


}

void sub15(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      dt->d1.a[i] = dt->d1.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


}
