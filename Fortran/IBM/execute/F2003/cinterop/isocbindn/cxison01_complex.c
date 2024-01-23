
/*
C code for testcase "fxison00.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

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

   a = 5.0f+I*5.0f;
   b = 10.0+I*10.0;

   sub1(&a,&b);

   if ( a != 10.0f+I*10.0f ) exit(21);
   if ( b != 20.0+I*20.0 ) exit(25);

/* Test 2 */

   a = 5.0f+I*5.0f;
   b = 10.0+I*10.0;

   sub2(a,b);

   if ( a != 5.0f+I*5.0f ) exit(29);
   if ( b != 10.0+I*10.0 ) exit(33);

/* Test 3 */

   a = 5.0f+I*5.0f;
   b = 10.0+I*10.0;

   sub3(&a,&b);

   if ( a != 5.0f+I*5.0f ) exit(37);
   if ( b != 10.0+I*10.0 ) exit(41);

/* Test 4 */

   a = 5.0f+I*5.0f;
   b = 10.0+I*10.0;

   sub4(a,b);

   if ( a != 5.0f+I*5.0f ) exit(45);
   if ( b != 10.0+I*10.0 ) exit(49);

/* Test 5 */

   a = 5.0f+I*5.0f;
   b = 10.0+I*10.0;

   sub5(&a,&b);

   if ( a != 5.0f+I*5.0f ) exit(53);
   if ( b != 10.0+I*10.0 ) exit(57);

/* Test 6 */

   a = 5.0f+I*5.0f;
   b = 10.0+I*10.0;

   sub6(a,b);

   if ( a != 5.0f+I*5.0f ) exit(61);
   if ( b != 10.0+I*10.0 ) exit(65);

   return 0;

}
