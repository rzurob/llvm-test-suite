
/*
	C code for testcase "fxisok00.f"
*/

#include <stdio.h>
#include <stdlib.h>

int main() {

   char fnt1(char *, signed char *);
   char fnt2(char, signed char);
   char fnt3(char *, signed char *);
   char fnt4(char, signed char);
   char fnt5(const char *, const signed char *);
   char fnt6(const char, const signed char);

   char a, ret;
   signed char b;

/* Test 1 */

   a = 'A';
   b = 'B';

   ret = fnt1(&a,&b);

   if ( a != 'C' ) exit(21);
   if ( b != 'D' ) exit(23);

/* Test 2 */

   a = 'A';
   b = 'B';

   ret = fnt2(a,b);

   if ( a != 'A' ) exit(25);
   if ( b != 'B' ) exit(27);

/* Test 3 */

   a = 'A';
   b = 'B';

   ret = fnt3(&a,&b);

   if ( a != 'A' ) exit(29);
   if ( b != 'B' ) exit(31);

/* Test 4 */

   a = 'A';
   b = 'B';

   ret = fnt4(a,b);

   if ( a != 'A' ) exit(33);
   if ( b != 'B' ) exit(35);

/* Test 5 */

   a = 'A';
   b = 'B';

   ret = fnt5(&a,&b);

   if ( a != 'A' ) exit(37);
   if ( b != 'B' ) exit(39);

/* Test 6 */

   a = 'A';
   b = 'B';

   ret = fnt6(a,b);

   if ( a != 'A' ) exit(41);
   if ( b != 'B' ) exit(43);

   return 0;

}
