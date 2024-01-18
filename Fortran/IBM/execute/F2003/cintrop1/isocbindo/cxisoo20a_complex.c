/*
        C code for testcase "fxisoo20a.f" and "fxisoo21a.f"
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

int fnt1(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(21);
      dt->a[i] = (long double)(i+2)+I*(long double)(i+2);
   }


   return 0;
}

int fnt2(struct dts0 dt) {
   int i; 


   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(25);
      dt.a[i] = (long double)(i+2)+I*(long double)(i+2);
   }


   return 0;
}

int fnt3(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(29);
      dt->a[i] = dt->a[i] + (long double)(i+1)+I*(long double)(i+1);
      if ( dt->d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(33);
      dt->d0.a[i] = dt->d0.a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


   return 0;
}

int fnt4(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(37);
      dt.a[i] = dt.a[i] + (long double)(i+1)+I*(long double)(i+1);
      if ( dt.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(41);
      dt.d0.a[i] = dt.d0.a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


   return 0;
}

int fnt5(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(45);
      dt->a[i] = dt->a[i] + (long double)(i+1)+I*(long double)(i+1);
      if ( dt->d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(49);
      dt->d1.a[i] = dt->d1.a[i] + (long double)(i+1)+I*(long double)(i+1);
      if ( dt->d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(53);
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


   return 0;
}

int fnt6(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(57);
      dt.a[i] = dt.a[i] + (long double)(i+1)+I*(long double)(i+1);
      if ( dt.d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(61);
      dt.d1.a[i] = dt.d1.a[i] + (long double)(i+1)+I*(long double)(i+1);
      if ( dt.d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(65);
      dt.d1.d0.a[i] = dt.d1.d0.a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


   return 0;
}

int fnt7(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(69);
   }


   return 0;
}

int fnt7a(const struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(73);
   }


   return 0;
}

int fnt8(struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(77);
   }


   return 0;
}

int fnt8a(const struct dts0 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(81);
   }


   return 0;
}

int fnt9(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(85);
      if ( dt->d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(89);
   }


   return 0;
}

int fnt9a(const struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(93);
      if ( dt->d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(97);
   }


   return 0;
}

int fnt10(struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(101);
      if ( dt.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(105);
   }


   return 0;
}

int fnt10a(const struct dts1 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(109);
      if ( dt.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(113);
   }


   return 0;
}

int fnt11(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(117);
      if ( dt->d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(121);
      if ( dt->d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(125);
   }


   return 0;
}

int fnt11a(const struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt->a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(129);
      if ( dt->d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(133);
      if ( dt->d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(137);
   }


   return 0;
}

int fnt12(struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(143);
      if ( dt.d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(147);
      if ( dt.d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(151);
   }


   return 0;
}

int fnt12a(const struct dts2 dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      if ( dt.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(155);
      if ( dt.d1.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(159);
      if ( dt.d1.d0.a[i] != (long double)(i+1)+I*(long double)(i+1) ) exit(163);
   }


   return 0;
}

int fnt13(struct dts0 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


   return 0;
}

int fnt14(struct dts1 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + (long double)(i+1)+I*(long double)(i+1);
      dt->d0.a[i] = dt->d0.a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


   return 0;
}

int fnt15(struct dts2 *dt) {
   int i; 

   for ( i = 0; i < 5; i++ ) {
      dt->a[i] = dt->a[i] + (long double)(i+1)+I*(long double)(i+1);
      dt->d1.a[i] = dt->d1.a[i] + (long double)(i+1)+I*(long double)(i+1);
      dt->d1.d0.a[i] = dt->d1.d0.a[i] + (long double)(i+1)+I*(long double)(i+1);
   }


   return 0;
}
