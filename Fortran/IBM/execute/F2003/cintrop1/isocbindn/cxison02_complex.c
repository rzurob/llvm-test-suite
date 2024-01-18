
/*
        C code for testcase "fxison02.f" and "fxison03.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

float _Complex fnt1(float _Complex *a, double _Complex *b) {

   if ( *a != 5.0f+I*5.0f ) exit(21);
   if ( *b != 10.0+I*10.0 ) exit(25);

   *a = *a + 5.0f+I*5.0f;
   *b = *b + 10.0+I*10.0;

   return 0;
}

float _Complex fnt2(float _Complex a, double _Complex b) {

   if ( a != 5.0f+I*5.0f ) exit(29);
   if ( b != 10.0+I*10.0 ) exit(33);

   a = a + 5.0f+I*5.0f;
   b = b + 10.0+I*10.0;

   return 0;
}

float _Complex fnt3(float _Complex *a, double _Complex *b) {

   if ( *a != 5.0f+I*5.0f ) exit(37);
   if ( *b != 10.0+I*10.0 ) exit(41);

   return 0;
}

float _Complex fnt4(float _Complex a, double _Complex b) {

   if ( a != 5.0f+I*5.0f ) exit(45);
   if ( b != 10.0+I*10.0 ) exit(49);

   return 0;
}

float _Complex fnt5(const float _Complex *a, const double _Complex *b) {

   if ( *a != 5.0f+I*5.0f ) exit(53);
   if ( *b != 10.0+I*10.0 ) exit(57);

   return 0;
}

float _Complex fnt6(const float _Complex a, const double _Complex b) {

   if ( a != 5.0f+I*5.0f ) exit(61);
   if ( b != 10.0+I*10.0 ) exit(65);

   return 0;
}
