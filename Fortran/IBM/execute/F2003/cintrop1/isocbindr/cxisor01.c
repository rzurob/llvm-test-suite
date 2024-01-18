
/*
	C code for testcase "fxisor00.f"
*/

#include <stdio.h>
#include <stdlib.h>


int main() {

   void sub1(_Bool *);
   void sub2(_Bool);
   void sub3(_Bool *);
   void sub4(_Bool);
   void sub5(const _Bool *);
   void sub6(const _Bool);

   _Bool a, ret;

/* Test 1 */

   a = 1;

   sub1(&a);

   if ( a != 0 ) exit(21);

/* Test 2 */

   a = 1;

   sub2(a);

   if ( a != 1 ) exit(23);

/* Test 3 */

   a = 1;

   sub3(&a);

   if ( a != 1 ) exit(25);

/* Test 4 */

   a = 1;

   sub4(a);

   if ( a != 1 ) exit(27);

/* Test 5 */

   a = 1;

   sub5(&a);

   if ( a != 1 ) exit(29);

/* Test 6 */

   a = 1;

   sub6(a);

   if ( a != 1 ) exit(31);

   return 0;

}
