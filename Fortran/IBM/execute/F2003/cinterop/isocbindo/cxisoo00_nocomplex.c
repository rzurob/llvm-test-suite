
/*
C code for testcase "fxisoo00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

int main() {

   long double _Complex fnt1(long double _Complex *);
   long double _Complex fnt2(long double _Complex);
   long double _Complex fnt3(long double _Complex *);
   long double _Complex fnt4(long double _Complex);
   long double _Complex fnt5(const long double _Complex *);
   long double _Complex fnt6(const long double _Complex);

   long double _Complex a, ret;

/* Test 1 */

   a = createcomplexl(5.0l,5.0l);

   ret = fnt1(&a);

   if ( a != createcomplexl(10.0l,10.0l) ) exit(23);

/* Test 2 */

   a = createcomplexl(5.0l,5.0l);

   ret = fnt2(a);

   if ( a != createcomplexl(5.0l,5.0l) ) exit(27);

/* Test 3 */

   a = createcomplexl(5.0l,5.0l);

   ret = fnt3(&a);

   if ( a != createcomplexl(5.0l,5.0l) ) exit(31);

/* Test 4 */

   a = createcomplexl(5.0l,5.0l);

   ret = fnt4(a);

   if ( a != createcomplexl(5.0l,5.0l) ) exit(35);

/* Test 5 */

   a = createcomplexl(5.0l,5.0l);

   ret = fnt5(&a);

   if ( a != createcomplexl(5.0l,5.0l) ) exit(39);

/* Test 6 */

   a = createcomplexl(5.0l,5.0l);

   ret = fnt6(a);

   if ( a != createcomplexl(5.0l,5.0l) ) exit(43);

   return 0;

}
