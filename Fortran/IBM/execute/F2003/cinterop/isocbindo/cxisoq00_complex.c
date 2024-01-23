
/*
C code for testcase "fxisoq00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

int main() {

   long double _Complex fnt1(long double _Complex *);
   long double _Complex fnt2(long double _Complex);
   long double _Complex fnt3(long double _Complex *);
   long double _Complex fnt4(long double _Complex);
   long double _Complex fnt5(const long double _Complex *);
   long double _Complex fnt6(const long double _Complex);

   long double _Complex a, ret;

/* Test 1 */

   a = 5.0l+I*5.0l;

   ret = fnt1(&a);

   if ( a != 10.0l+I*10.0l ) exit(21);

/* Test 2 */

   a = 5.0l+I*5.0l;

   ret = fnt2(a);

   if ( a != 5.0l+I*5.0l ) exit(25);

/* Test 3 */

   a = 5.0l+I*5.0l;

   ret = fnt3(&a);

   if ( a != 5.0l+I*5.0l ) exit(29);

/* Test 4 */

   a = 5.0l+I*5.0l;

   ret = fnt4(a);

   if ( a != 5.0l+I*5.0l ) exit(33);

/* Test 5 */

   a = 5.0l+I*5.0l;

   ret = fnt5(&a);

   if ( a != 5.0l+I*5.0l ) exit(37);

/* Test 6 */

   a = 5.0l+I*5.0l;

   ret = fnt6(a);

   if ( a != 5.0l+I*5.0l ) exit(41);

   return 0;

}
