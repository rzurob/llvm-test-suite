
/*
	C code for testcase "fxisom00.f"
*/

#include <stdio.h>
#include <stdlib.h>

int main() {

   long double fnt1(long double *);
   long double fnt2(long double);
   long double fnt3(long double *);
   long double fnt4(long double);
   long double fnt5(const long double *);
   long double fnt6(const long double);

   long double a, ret;

/* Test 1 */

   a = 5.0l;

   ret = fnt1(&a);

   if ( a != 10.0l ) exit(21);

/* Test 2 */

   a = 5.0l;

   ret = fnt2(a);

   if ( a != 5.0l ) exit(23);

/* Test 3 */

   a = 5.0l;

   ret = fnt3(&a);

   if ( a != 5.0l ) exit(25);

/* Test 4 */

   a = 5.0l;

   ret = fnt4(a);

   if ( a != 5.0l ) exit(27);

/* Test 5 */

   a = 5.0l;

   ret = fnt5(&a);

   if ( a != 5.0l ) exit(29);

/* Test 6 */

   a = 5.0l;

   ret = fnt6(a);

   if ( a != 5.0l ) exit(31);

   return 0;

}
