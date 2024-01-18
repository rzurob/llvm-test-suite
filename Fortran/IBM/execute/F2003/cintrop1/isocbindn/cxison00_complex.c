
/*
C code for testcase "fxison00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

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

   a = 5.0f+I*5.0f;
   b = 10.0+I*10.0;

   ret = fnt1(&a,&b);

   if ( a != 10.0f+I*10.0f ) exit(21);
   if ( b != 20.0+I*20.0 ) exit(25);

/* Test 2 */

   a = 5.0f+I*5.0f;
   b = 10.0+I*10.0;

   ret = fnt2(a,b);

   if ( a != 5.0f+I*5.0f ) exit(29);
   if ( b != 10.0+I*10.0 ) exit(33);

/* Test 3 */

   a = 5.0f+I*5.0f;
   b = 10.0+I*10.0;

   ret = fnt3(&a,&b);

   if ( a != 5.0f+I*5.0f ) exit(37);
   if ( b != 10.0+I*10.0 ) exit(41);

/* Test 4 */

   a = 5.0f+I*5.0f;
   b = 10.0+I*10.0;

   ret = fnt4(a,b);

   if ( a != 5.0f+I*5.0f ) exit(45);
   if ( b != 10.0+I*10.0 ) exit(49);

/* Test 5 */

   a = 5.0f+I*5.0f;
   b = 10.0+I*10.0;

   ret = fnt5(&a,&b);

   if ( a != 5.0f+I*5.0f ) exit(53);
   if ( b != 10.0+I*10.0 ) exit(57);

/* Test 6 */

   a = 5.0f+I*5.0f;
   b = 10.0+I*10.0;

   ret = fnt6(a,b);

   if ( a != 5.0f+I*5.0f ) exit(61);
   if ( b != 10.0+I*10.0 ) exit(65);

   return 0;

}
