
/*
	C code for testcase "fxisop00.f"
*/

#include <stdio.h>
#include <stdlib.h>

int main() {

   void sub1(long double *);
   void sub2(long double);
   void sub3(long double *);
   void sub4(long double);
   void sub5(const long double *);
   void sub6(const long double);

   long double a, ret;

/* Test 1 */

   a = 5.0l;

   sub1(&a);

   if ( a != 10.0l ) exit(21);

/* Test 2 */

   a = 5.0l;

   sub2(a);

   if ( a != 5.0l ) exit(23);

/* Test 3 */

   a = 5.0l;

   sub3(&a);

   if ( a != 5.0l ) exit(25);

/* Test 4 */

   a = 5.0l;

   sub4(a);

   if ( a != 5.0l ) exit(27);

/* Test 5 */

   a = 5.0l;

   sub5(&a);

   if ( a != 5.0l ) exit(29);

/* Test 6 */

   a = 5.0l;

   sub6(a);

   if ( a != 5.0l ) exit(31);

   return 0;

}
