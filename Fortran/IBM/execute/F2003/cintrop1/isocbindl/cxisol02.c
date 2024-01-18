
/*
        C code for testcase "fxisol02.f" and "fxisol03.f"
*/

#include <stdio.h>
#include <stdlib.h>


float fnt1(float *a, double *b) {

   if ( *a != 5.0f ) exit(21);
   if ( *b != 10.0 ) exit(23);

   *a = *a + 5.0f;
   *b = *b + 10.0;

   return 0;
}

float fnt2(float a, double b) {

   if ( a != 5.0f ) exit(25);
   if ( b != 10.0 ) exit(27);

   a = a + 5.0f;
   b = b + 10.0;

   return 0;
}

float fnt3(float *a, double *b) {

   if ( *a != 5.0f ) exit(29);
   if ( *b != 10.0 ) exit(31);

   return 0;
}

float fnt4(float a, double b) {

   if ( a != 5.0f ) exit(33);
   if ( b != 10.0 ) exit(35);

   return 0;
}

float fnt5(const float *a, const double *b) {

   if ( *a != 5.0f ) exit(37);
   if ( *b != 10.0 ) exit(39);

   return 0;
}

float fnt6(const float a, const double b) {

   if ( a != 5.0f ) exit(41);
   if ( b != 10.0 ) exit(43);

   return 0;
}
