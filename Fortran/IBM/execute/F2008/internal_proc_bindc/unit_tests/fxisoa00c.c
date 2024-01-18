
/*
	C code for testcase "fxisoa00.f"
*/

#include <stdio.h>
#include <stdlib.h>


int main() {

   int fnt1(int *, short *);
   int fnt2(int, short);
   int fnt3(int *, short *);
   int fnt4(int, short);
   int fnt5(const int *, const short *);
   int fnt6(const int, const short);

   int a, ret;
   short b;

/* Test 1 */

   a = 5;
   b = 10;

   //ret = fnt1(&a,&b);
   ret = some1(&a,&b);

   if ( a != 10 ) exit(21);
   if ( b != 20 ) exit(23);
   if ( ret != 120 ) exit(25);

/* Test 2 */

   a = 5;
   b = 10;

   ret = fnt2(a,b);

   if ( a != 5 ) exit(27);
   if ( b != 10 ) exit(29);
   if ( ret != 120 ) exit(31);

/* Test 3 */

   a = 5;
   b = 10;

   ret = fnt3(&a,&b);

   if ( a != 5 ) exit(33);
   if ( b != 10 ) exit(35);
   if ( ret != 35 ) exit(37);

/* Test 4 */

   a = 5;
   b = 10;

   ret = fnt4(a,b);

   if ( a != 5 ) exit(39);
   if ( b != 10 ) exit(41);
   if ( ret != 35 ) exit(43);

/* Test 5 */

   a = 5;
   b = 10;

   ret = fnt5(&a,&b);

   if ( a != 5 ) exit(45);
   if ( b != 10 ) exit(47);
   if ( ret != 35 ) exit(49);

/* Test 6 */

   a = 5;
   b = 10;

   ret = fnt6(a,b);

   if ( a != 5 ) exit(51);
   if ( b != 10 ) exit(53);
   if ( ret != 35 ) exit(55);

   return 0;

}
