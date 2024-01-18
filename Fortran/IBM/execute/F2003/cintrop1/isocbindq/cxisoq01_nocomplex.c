
/*
C code for testcase "fxisoq00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

int main() {

   void sub1(long double _Complex *);
   void sub2(long double _Complex);
   void sub3(long double _Complex *);
   void sub4(long double _Complex);
   void sub5(const long double _Complex *);
   void sub6(const long double _Complex);

   long double _Complex a, ret;

/* Test 1 */

   a = createcomplexl(5.0l,5.0l);

   sub1(&a);

   if ( a != createcomplexl(10.0l,10.0l) ) exit(23);

/* Test 2 */

   a = createcomplexl(5.0l,5.0l);

   sub2(a);

   if ( a != createcomplexl(5.0l,5.0l) ) exit(27);

/* Test 3 */

   a = createcomplexl(5.0l,5.0l);

   sub3(&a);

   if ( a != createcomplexl(5.0l,5.0l) ) exit(31);

/* Test 4 */

   a = createcomplexl(5.0l,5.0l);

   sub4(a);

   if ( a != createcomplexl(5.0l,5.0l) ) exit(35);

/* Test 5 */

   a = createcomplexl(5.0l,5.0l);

   sub5(&a);

   if ( a != createcomplexl(5.0l,5.0l) ) exit(39);

/* Test 6 */

   a = createcomplexl(5.0l,5.0l);

   sub6(a);

   if ( a != createcomplexl(5.0l,5.0l) ) exit(43);

   return 0;

}
