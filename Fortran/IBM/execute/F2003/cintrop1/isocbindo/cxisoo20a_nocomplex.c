/*
        C code for testcase "fxisoo20a.f" and "fxisoo21a.f"
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

int fnt1(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(23);
      dt->a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
   }


   return 0;
}

int fnt2(struct dts0 dt) {
   int i; 


   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(27);
      dt.a[i] = createcomplexl((long double)(i+2),(long double)(i+2));
   }


   return 0;
}

int fnt3(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(31);
      dt->a[i] = dt->a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      if ( dt->d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(35);
      dt->d0.a[i] = dt->d0.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


   return 0;
}

int fnt4(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(39);
      dt.a[i] = dt.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      if ( dt.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(43);
      dt.d0.a[i] = dt.d0.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


   return 0;
}

int fnt5(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(47);
      dt->a[i] = dt->a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      if ( dt->d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(51);
      dt->d1.a[i] = dt->d1.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      if ( dt->d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(55);
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


   return 0;
}

int fnt6(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(59);
      dt.a[i] = dt.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      if ( dt.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(63);
      dt.d1.a[i] = dt.d1.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      if ( dt.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(67);
      dt.d1.d0.a[i] = dt.d1.d0.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


   return 0;
}

int fnt7(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(71);
   }


   return 0;
}

int fnt7a(const struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(75);
   }


   return 0;
}

int fnt8(struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(79);
   }


   return 0;
}

int fnt8a(const struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(83);
   }


   return 0;
}

int fnt9(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(87);
      if ( dt->d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(91);
   }


   return 0;
}

int fnt9a(const struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(95);
      if ( dt->d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(99);
   }


   return 0;
}

int fnt10(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(103);
      if ( dt.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(107);
   }


   return 0;
}

int fnt10a(const struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(111);
      if ( dt.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(115);
   }


   return 0;
}

int fnt11(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(119);
      if ( dt->d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(123);
      if ( dt->d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(127);
   }


   return 0;
}

int fnt11a(const struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(131);
      if ( dt->d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(135);
      if ( dt->d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(141);
   }


   return 0;
}

int fnt12(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(145);
      if ( dt.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(149);
      if ( dt.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(153);
   }


   return 0;
}

int fnt12a(const struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(157);
      if ( dt.d1.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(161);
      if ( dt.d1.d0.a[i] != createcomplexl((long double)(i+1),(long double)(i+1)) ) exit(165);
   }


   return 0;
}

int fnt13(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


   return 0;
}

int fnt14(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      dt->d0.a[i] = dt->d0.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


   return 0;
}

int fnt15(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      dt->d1.a[i] = dt->d1.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + createcomplexl((long double)(i+1),(long double)(i+1));
   }


   return 0;
}
