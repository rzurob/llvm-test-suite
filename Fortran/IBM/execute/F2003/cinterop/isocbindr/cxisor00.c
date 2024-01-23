
/*
	C code for testcase "fxisor00.f"
*/

#include <stdio.h>
#include <stdlib.h>


int main() {

   _Bool fnt1(_Bool *);
   _Bool fnt2(_Bool);
   _Bool fnt3(_Bool *);
   _Bool fnt4(_Bool);
   _Bool fnt5(const _Bool *);
   _Bool fnt6(const _Bool);

   _Bool a, ret;

/* Test 1 */

   a = 1;

   ret = fnt1(&a);

   if ( a != 0 ) exit(21);
   if ( ret != 0 ) exit(23);

/* Test 2 */

   a = 1;

   ret = fnt2(a);

   if ( a != 1 ) exit(25);
   if ( ret != 0 ) exit(27);

/* Test 3 */

   a = 1;

   ret = fnt3(&a);

   if ( a != 1 ) exit(29);
   if ( ret != 0 ) exit(31);

/* Test 4 */

   a = 1;

   ret = fnt4(a);

   if ( a != 1 ) exit(33);
   if ( ret != 0 ) exit(35);

/* Test 5 */

   a = 1;

   ret = fnt5(&a);

   if ( a != 1 ) exit(37);
   if ( ret != 0 ) exit(39);

/* Test 6 */

   a = 1;

   ret = fnt6(a);

   if ( a != 1 ) exit(41);
   if ( ret != 0 ) exit(43);

   return 0;

}
