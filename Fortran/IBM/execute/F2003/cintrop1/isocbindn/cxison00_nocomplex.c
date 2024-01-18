
/*
C code for testcase "fxison00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

int main() {

   float _Complex fnt1(float _Complex *, double _Complex *);
   float _Complex fnt2(float _Complex, double _Complex);
   float _Complex fnt3(float _Complex *, double _Complex *);
   float _Complex fnt4(float _Complex, double _Complex);
   float _Complex fnt5(const float _Complex *, const double _Complex *);
   float _Complex fnt6(const float _Complex, const double _Complex);

   float _Complex a, ret;
   double _Complex b;

/* Test 1 */

   a = createcomplexf(5.0f,5.0f);
   b = createcomplex(10.0,10.0);

   ret = fnt1(&a,&b);

   if ( a != createcomplexf(10.0f,10.0f) ) exit(23);
   if ( b != createcomplex(20.0,20.0) ) exit(27);

/* Test 2 */

   a = createcomplexf(5.0f,5.0f);
   b = createcomplex(10.0,10.0);

   ret = fnt2(a,b);

   if ( a != createcomplexf(5.0f,5.0f) ) exit(31);
   if ( b != createcomplex(10.0,10.0) ) exit(35);

/* Test 3 */

   a = createcomplexf(5.0f,5.0f);
   b = createcomplex(10.0,10.0);

   ret = fnt3(&a,&b);

   if ( a != createcomplexf(5.0f,5.0f) ) exit(39);
   if ( b != createcomplex(10.0,10.0) ) exit(43);

/* Test 4 */

   a = createcomplexf(5.0f,5.0f);
   b = createcomplex(10.0,10.0);

   ret = fnt4(a,b);

   if ( a != createcomplexf(5.0f,5.0f) ) exit(47);
   if ( b != createcomplex(10.0,10.0) ) exit(51);

/* Test 5 */

   a = createcomplexf(5.0f,5.0f);
   b = createcomplex(10.0,10.0);

   ret = fnt5(&a,&b);

   if ( a != createcomplexf(5.0f,5.0f) ) exit(55);
   if ( b != createcomplex(10.0,10.0) ) exit(59);

/* Test 6 */

   a = createcomplexf(5.0f,5.0f);
   b = createcomplex(10.0,10.0);

   ret = fnt6(a,b);

   if ( a != createcomplexf(5.0f,5.0f) ) exit(63);
   if ( b != createcomplex(10.0,10.0) ) exit(67);

   return 0;

}
