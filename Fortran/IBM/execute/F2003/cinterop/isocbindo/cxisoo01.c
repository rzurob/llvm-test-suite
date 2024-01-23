
/*
	C code for testcase "fxisoo00.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

int main() {

   void sub1(long double _Complex *);
   void sub2(long double _Complex);
   void sub3(long double _Complex *);
   void sub4(long double _Complex);
   void sub5(const long double _Complex *);
   void sub6(const long double _Complex);

   long double _Complex a, ret;

/* Test 1 */

#ifdef CMPLX
   a = 5.0l+I*5.0l;
#else
   a = createcomplexl(5.0l,5.0l);
#endif

   sub1(&a);

#ifdef CMPLX
   if ( a != 10.0l+I*10.0l ) exit(21);
#else
   if ( a != createcomplexl(10.0l,10.0l) ) exit(23);
#endif

/* Test 2 */

#ifdef CMPLX
   a = 5.0l+I*5.0l;
#else
   a = createcomplexl(5.0l,5.0l);
#endif

   sub2(a);

#ifdef CMPLX
   if ( a != 5.0l+I*5.0l ) exit(25);
#else
   if ( a != createcomplexl(5.0l,5.0l) ) exit(27);
#endif

/* Test 3 */

#ifdef CMPLX
   a = 5.0l+I*5.0l;
#else
   a = createcomplexl(5.0l,5.0l);
#endif

   sub3(&a);

#ifdef CMPLX
   if ( a != 5.0l+I*5.0l ) exit(29);
#else
   if ( a != createcomplexl(5.0l,5.0l) ) exit(31);
#endif

/* Test 4 */

#ifdef CMPLX
   a = 5.0l+I*5.0l;
#else
   a = createcomplexl(5.0l,5.0l);
#endif

   sub4(a);

#ifdef CMPLX
   if ( a != 5.0l+I*5.0l ) exit(33);
#else
   if ( a != createcomplexl(5.0l,5.0l) ) exit(35);
#endif

/* Test 5 */

#ifdef CMPLX
   a = 5.0l+I*5.0l;
#else
   a = createcomplexl(5.0l,5.0l);
#endif

   sub5(&a);

#ifdef CMPLX
   if ( a != 5.0l+I*5.0l ) exit(37);
#else
   if ( a != createcomplexl(5.0l,5.0l) ) exit(39);
#endif

/* Test 6 */

#ifdef CMPLX
   a = 5.0l+I*5.0l;
#else
   a = createcomplexl(5.0l,5.0l);
#endif

   sub6(a);

#ifdef CMPLX
   if ( a != 5.0l+I*5.0l ) exit(41);
#else
   if ( a != createcomplexl(5.0l,5.0l) ) exit(43);
#endif

   return 0;

}
