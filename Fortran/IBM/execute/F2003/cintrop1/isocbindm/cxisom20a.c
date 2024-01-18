/*
        C code for testcase "fxisom20a.f" and "fxisom21a.f"
*/

#include <stdio.h>
#include <stdlib.h>

struct dts0 {
   long double a[5];
};

struct dts1 {
   long double a[5];
   struct dts0 d0;
};

struct dts2 {
   long double a[5];
   struct dts1 d1;
};

int fnt1(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1) ) exit(21);
      dt->a[i] = i+2;
   }


   return 0;
}

int fnt2(struct dts0 dt) {
   int i; 


   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1) ) exit(23);
      dt.a[i] = i+2;
   }


   return 0;
}

int fnt3(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1) ) exit(25);
      dt->a[i] = dt->a[i]+i+1;
      if ( dt->d0.a[i] != (long double)(i+1) ) exit(27);
      dt->d0.a[i] = dt->d0.a[i]+i+1;
   }


   return 0;
}

int fnt4(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1) ) exit(29);
      dt.a[i] = dt.a[i]+i+1;
      if ( dt.d0.a[i] != (long double)(i+1) ) exit(31);
      dt.d0.a[i] = dt.d0.a[i]+i+1;
   }


   return 0;
}

int fnt5(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1) ) exit(33);
      dt->a[i] = dt->a[i]+i+1;
      if ( dt->d1.a[i] != (long double)(i+1) ) exit(35);
      dt->d1.a[i] = dt->d1.a[i]+i+1;
      if ( dt->d1.d0.a[i] != (long double)(i+1) ) exit(37);
      dt->d1.d0.a[i] = dt->d1.d0.a[i]+i+1;
   }


   return 0;
}

int fnt6(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1) ) exit(39);
      dt.a[i] = dt.a[i]+i+1;
      if ( dt.d1.a[i] != (long double)(i+1) ) exit(41);
      dt.d1.a[i] = dt.d1.a[i]+i+1;
      if ( dt.d1.d0.a[i] != (long double)(i+1) ) exit(43);
      dt.d1.d0.a[i] = dt.d1.d0.a[i]+i+1;
   }


   return 0;
}

int fnt7(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1) ) exit(45);
   }


   return 0;
}

int fnt7a(const struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1) ) exit(47);
   }


   return 0;
}

int fnt8(struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1) ) exit(49);
   }


   return 0;
}

int fnt8a(const struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1) ) exit(51);
   }


   return 0;
}

int fnt9(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1) ) exit(53);
      if ( dt->d0.a[i] != (long double)(i+1) ) exit(55);
   }


   return 0;
}

int fnt9a(const struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1) ) exit(57);
      if ( dt->d0.a[i] != (long double)(i+1) ) exit(59);
   }


   return 0;
}

int fnt10(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1) ) exit(61);
      if ( dt.d0.a[i] != (long double)(i+1) ) exit(63);
   }


   return 0;
}

int fnt10a(const struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1) ) exit(65);
      if ( dt.d0.a[i] != (long double)(i+1) ) exit(67);
   }


   return 0;
}

int fnt11(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1) ) exit(69);
      if ( dt->d1.a[i] != (long double)(i+1) ) exit(71);
      if ( dt->d1.d0.a[i] != (long double)(i+1) ) exit(73);
   }


   return 0;
}

int fnt11a(const struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1) ) exit(75);
      if ( dt->d1.a[i] != (long double)(i+1) ) exit(77);
      if ( dt->d1.d0.a[i] != (long double)(i+1) ) exit(79);
   }


   return 0;
}

int fnt12(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1) ) exit(81);
      if ( dt.d1.a[i] != (long double)(i+1) ) exit(83);
      if ( dt.d1.d0.a[i] != (long double)(i+1) ) exit(85);
   }


   return 0;
}

int fnt12a(const struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1) ) exit(87);
      if ( dt.d1.a[i] != (long double)(i+1) ) exit(89);
      if ( dt.d1.d0.a[i] != (long double)(i+1) ) exit(91);
   }


   return 0;
}

int fnt13(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i]+i+1;
   }


   return 0;
}

int fnt14(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i]+i+1;
      dt->d0.a[i] = dt->d0.a[i]+i+1;
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


   return 0;
}
