
/*
C code for testcase "fxisoo19b.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

struct dtd0 {
   long double _Complex a[5][10];
};

struct dtd1 {
   long double _Complex a[5][10];
   struct dtd0 d0;
};

struct dtd2 {
   long double _Complex a[5][10];
   struct dtd1 d1;
};

void initdtd0(struct dtd0 *);
void initdtd1(struct dtd1 *);
void initdtd2(struct dtd2 *);

int main() {

   void sub1(struct dtd0 *);
   void sub2(struct dtd0);
   void sub3(struct dtd1 *);
   void sub4(struct dtd1);
   void sub5(struct dtd2 *);
   void sub6(struct dtd2);
   void sub7(struct dtd0 *);
   void sub7a(const struct dtd0 *);
   void sub8(struct dtd0);
   void sub8a(const struct dtd0);
   void sub9(struct dtd1 *);
   void sub9a(const struct dtd1 *);
   void sub10(struct dtd1);
   void sub10a(const struct dtd1);
   void sub11(struct dtd2 *);
   void sub11a(const struct dtd2 *);
   void sub12(struct dtd2);
   void sub12a(const struct dtd2);
   void sub13(struct dtd0 *);
   void sub14(struct dtd1 *);
   void sub15(struct dtd2 *);

   struct dtd0 dta;
   struct dtd1 dtb;
   struct dtd2 dtc;
   int i, j, ret;

/* Test 1 */

   initdtd0(&dta);

   sub1(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(23);
      }
   }


/* Test 2 */

   initdtd0(&dta);

   sub2(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(27);
      }
   }


/* Test 3 */

   initdtd1(&dtb);

   sub3(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(31);
         if ( dtb.d0.a[i][j] != createcomplexl((long double)((i+j+2)*2-1),(long double)((i+j+2)*2-1)) ) exit(35);
      }
   }


/* Test 4 */

   initdtd1(&dtb);

   sub4(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(39);
         if ( dtb.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(43);
      }
   }


/* Test 5 */

   initdtd2(&dtc);

   sub5(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(47);
         if ( dtc.d1.a[i][j] != createcomplexl((long double)((i+j+2)*2-1),(long double)((i+j+2)*2-1)) ) exit(51);
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)((i+j+2)*2-1),(long double)((i+j+2)*2-1)) ) exit(55);
      }
   }


/* Test 6 */

   initdtd2(&dtc);

   sub6(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(59);
         if ( dtc.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(63);
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(67);
      }
   }


/* Test 7 */

   initdtd0(&dta);

   sub7(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(71);
      }
   }


/* Test 7a */

   initdtd0(&dta);

   sub7a(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(75);
      }
   }


/* Test 8 */

   initdtd0(&dta);

   sub8(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(79);
      }
   }


/* Test 8a */

   initdtd0(&dta);

   sub8a(dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(83);
      }
   }


/* Test 9 */

   initdtd1(&dtb);

   sub9(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(87);
         if ( dtb.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(91);
      }
   }


/* Test 9a */

   initdtd1(&dtb);

   sub9a(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(95);
         if ( dtb.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(99);
      }
   }


/* Test 10 */

   initdtd1(&dtb);

   sub10(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(103);
         if ( dtb.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(107);
      }
   }


/* Test 10a */

   initdtd1(&dtb);

   sub10a(dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(111);
         if ( dtb.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(115);
      }
   }


/* Test 11 */

   initdtd2(&dtc);

   sub11(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(119);
         if ( dtc.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(123);
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(127);
      }
   }


/* Test 11a */

   initdtd2(&dtc);

   sub11a(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(131);
         if ( dtc.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(135);
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(141);
      }
   }


/* Test 12 */

   initdtd2(&dtc);

   sub12(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(145);
         if ( dtc.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(149);
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(153);
      }
   }


/* Test 12a */

   initdtd2(&dtc);

   sub12a(dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(157);
         if ( dtc.d1.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(161);
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)(i+j+1),(long double)(i+j+1)) ) exit(165);
      }
   }


/* Test 13 */

   initdtd0(&dta);

   sub13(&dta);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dta.a[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(169);
      }
   }


/* Test 14 */

   initdtd1(&dtb);

   sub14(&dtb);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtb.a[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(173);
         if ( dtb.d0.a[i][j] != createcomplexl((long double)((i+j+2)*2-1),(long double)((i+j+2)*2-1)) ) exit(177);
      }
   }


/* Test 15 */

   initdtd2(&dtc);

   sub15(&dtc);

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         if ( dtc.a[i][j] != createcomplexl((long double)(i+j+2),(long double)(i+j+2)) ) exit(181);
         if ( dtc.d1.a[i][j] != createcomplexl((long double)((i+j+2)*2-1),(long double)((i+j+2)*2-1)) ) exit(185);
         if ( dtc.d1.d0.a[i][j] != createcomplexl((long double)((i+j+2)*2-1),(long double)((i+j+2)*2-1)) ) exit(189);
      }
   }


   return 0;
}

void initdtd0(struct dtd0 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         x->a[i][j] = createcomplexl((long double)(i+j+1),(long double)(i+j+1));
      }
   }


}

void initdtd1(struct dtd1 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         x->a[i][j] = createcomplexl((long double)(i+j+1),(long double)(i+j+1));
      }
   }


   initdtd0(&x->d0);

}

void initdtd2(struct dtd2 *x) {

   int i, j;

   for ( i = 0; i < 5; i++ ) {
      for ( j = 0; j < 10; j++ ) {
         x->a[i][j] = createcomplexl((long double)(i+j+1),(long double)(i+j+1));
      }
   }


   initdtd1(&x->d1);

}
