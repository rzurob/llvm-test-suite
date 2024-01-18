
/*
	C code for testcase "fxisol00.f"
*/

#include <stdio.h>
#include <stdlib.h>


int main() {

   float fnt1(float *, double *);
   float fnt2(float, double);
   float fnt3(float *, double *);
   float fnt4(float, double);
   float fnt5(const float *, const double *);
   float fnt6(const float, const double);

   float a, ret;
   double b;

/* Test 1 */

   a = 5.0f;
   b = 10.0;

   ret = fnt1(&a,&b);

   if ( a != 10.0f ) exit(21);
   if ( b != 20.0 ) exit(23);

/* Test 2 */

   a = 5.0f;
   b = 10.0;

   ret = fnt2(a,b);

   if ( a != 5.0f ) exit(25);
   if ( b != 10.0 ) exit(27);

/* Test 3 */

   a = 5.0f;
   b = 10.0;

   ret = fnt3(&a,&b);

   if ( a != 5.0f ) exit(29);
   if ( b != 10.0 ) exit(31);

/* Test 4 */

   a = 5.0f;
   b = 10.0;

   ret = fnt4(a,b);

   if ( a != 5.0f ) exit(33);
   if ( b != 10.0 ) exit(35);

/* Test 5 */

   a = 5.0f;
   b = 10.0;

   ret = fnt5(&a,&b);

   if ( a != 5.0f ) exit(37);
   if ( b != 10.0 ) exit(39);

/* Test 6 */

   a = 5.0f;
   b = 10.0;

   ret = fnt6(a,b);

   if ( a != 5.0f ) exit(41);
   if ( b != 10.0 ) exit(43);

   return 0;

}
