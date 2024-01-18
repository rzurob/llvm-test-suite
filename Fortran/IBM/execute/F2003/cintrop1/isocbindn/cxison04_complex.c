
/*
        C code for testcase "fxison04.f" and "fxison05.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include <complex.h>

void sub1(float _Complex *a, double _Complex *b) {

   if ( *a != 5.0f+I*5.0f ) exit(21);
   if ( *b != 10.0+I*10.0 ) exit(25);

   *a = *a + 5.0f+I*5.0f;
   *b = *b + 10.0+I*10.0;

}

void sub2(float _Complex a, double _Complex b) {

   if ( a != 5.0f+I*5.0f ) exit(29);
   if ( b != 10.0+I*10.0 ) exit(33);

   a = a + 5.0f+I*5.0f;
   b = b + 10.0+I*10.0;

}

void sub3(float _Complex *a, double _Complex *b) {

   if ( *a != 5.0f+I*5.0f ) exit(37);
   if ( *b != 10.0+I*10.0 ) exit(41);

}

void sub4(float _Complex a, double _Complex b) {

   if ( a != 5.0f+I*5.0f ) exit(45);
   if ( b != 10.0+I*10.0 ) exit(49);

}

void sub5(const float _Complex *a, const double _Complex *b) {

   if ( *a != 5.0f+I*5.0f ) exit(53);
   if ( *b != 10.0+I*10.0 ) exit(57);

}

void sub6(const float _Complex a, const double _Complex b) {

   if ( a != 5.0f+I*5.0f ) exit(61);
   if ( b != 10.0+I*10.0 ) exit(65);

}
