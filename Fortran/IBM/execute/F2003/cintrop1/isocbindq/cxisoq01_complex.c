
/*
C code for testcase "fxisoq00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

int main() {

   void sub1(long double _Complex *);
   void sub2(long double _Complex);
   void sub3(long double _Complex *);
   void sub4(long double _Complex);
   void sub5(const long double _Complex *);
   void sub6(const long double _Complex);

   long double _Complex a, ret;

/* Test 1 */

   a = 5.0l+I*5.0l;

   sub1(&a);

   if ( a != 10.0l+I*10.0l ) exit(21);

/* Test 2 */

   a = 5.0l+I*5.0l;

   sub2(a);

   if ( a != 5.0l+I*5.0l ) exit(25);

/* Test 3 */

   a = 5.0l+I*5.0l;

   sub3(&a);

   if ( a != 5.0l+I*5.0l ) exit(29);

/* Test 4 */

   a = 5.0l+I*5.0l;

   sub4(a);

   if ( a != 5.0l+I*5.0l ) exit(33);

/* Test 5 */

   a = 5.0l+I*5.0l;

   sub5(&a);

   if ( a != 5.0l+I*5.0l ) exit(37);

/* Test 6 */

   a = 5.0l+I*5.0l;

   sub6(a);

   if ( a != 5.0l+I*5.0l ) exit(41);

   return 0;

}
