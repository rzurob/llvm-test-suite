
/*
C code for testcase "fxison00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

int main() {

   void sub1(float _Complex *, double _Complex *);
   void sub2(float _Complex, double _Complex);
   void sub3(float _Complex *, double _Complex *);
   void sub4(float _Complex, double _Complex);
   void sub5(const float _Complex *, const double _Complex *);
   void sub6(const float _Complex, const double _Complex);

   float _Complex a, ret;
   double _Complex b;

/* Test 1 */

   a = createcomplexf(5.0f,5.0f);
   b = createcomplex(10.0,10.0);

   sub1(&a,&b);

   if ( a != createcomplexf(10.0f,10.0f) ) exit(23);
   if ( b != createcomplex(20.0,20.0) ) exit(27);

/* Test 2 */

   a = createcomplexf(5.0f,5.0f);
   b = createcomplex(10.0,10.0);

   sub2(a,b);

   if ( a != createcomplexf(5.0f,5.0f) ) exit(31);
   if ( b != createcomplex(10.0,10.0) ) exit(35);

/* Test 3 */

   a = createcomplexf(5.0f,5.0f);
   b = createcomplex(10.0,10.0);

   sub3(&a,&b);

   if ( a != createcomplexf(5.0f,5.0f) ) exit(39);
   if ( b != createcomplex(10.0,10.0) ) exit(43);

/* Test 4 */

   a = createcomplexf(5.0f,5.0f);
   b = createcomplex(10.0,10.0);

   sub4(a,b);

   if ( a != createcomplexf(5.0f,5.0f) ) exit(47);
   if ( b != createcomplex(10.0,10.0) ) exit(51);

/* Test 5 */

   a = createcomplexf(5.0f,5.0f);
   b = createcomplex(10.0,10.0);

   sub5(&a,&b);

   if ( a != createcomplexf(5.0f,5.0f) ) exit(55);
   if ( b != createcomplex(10.0,10.0) ) exit(59);

/* Test 6 */

   a = createcomplexf(5.0f,5.0f);
   b = createcomplex(10.0,10.0);

   sub6(a,b);

   if ( a != createcomplexf(5.0f,5.0f) ) exit(63);
   if ( b != createcomplex(10.0,10.0) ) exit(67);

   return 0;

}
