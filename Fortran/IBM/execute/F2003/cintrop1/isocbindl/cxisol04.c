
/*
        C code for testcase "fxisol04.f" and "fxisol05.f"
*/

#include <stdio.h>
#include <stdlib.h>


void sub1(float *a, double *b) {

   if ( *a != 5.0f ) exit(21);
   if ( *b != 10.0 ) exit(23);

   *a = *a + 5.0f;
   *b = *b + 10.0;

}

void sub2(float a, double b) {

   if ( a != 5.0f ) exit(25);
   if ( b != 10.0 ) exit(27);

   a = a + 5.0f;
   b = b + 10.0;

}

void sub3(float *a, double *b) {

   if ( *a != 5.0f ) exit(29);
   if ( *b != 10.0 ) exit(31);

}

void sub4(float a, double b) {

   if ( a != 5.0f ) exit(33);
   if ( b != 10.0 ) exit(35);

}

void sub5(const float *a, const double *b) {

   if ( *a != 5.0f ) exit(37);
   if ( *b != 10.0 ) exit(39);

}

void sub6(const float a, const double b) {

   if ( a != 5.0f ) exit(41);
   if ( b != 10.0 ) exit(43);

}
