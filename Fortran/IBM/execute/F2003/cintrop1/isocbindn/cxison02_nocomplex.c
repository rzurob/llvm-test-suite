
/*
        C code for testcase "fxison02.f" and "fxison03.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

float _Complex fnt1(float _Complex *a, double _Complex *b) {

   if ( *a != createcomplexf(5.0f,5.0f) ) exit(23);
   if ( *b != createcomplex(10.0,10.0) ) exit(27);

   *a = *a + createcomplexf(5.0f,5.0f);
   *b = *b + createcomplex(10.0,10.0);

   return 0;
}

float _Complex fnt2(float _Complex a, double _Complex b) {

   if ( a != createcomplexf(5.0f,5.0f) ) exit(31);
   if ( b != createcomplex(10.0,10.0) ) exit(35);

   a = a + createcomplexf(5.0f,5.0f);
   b = b + createcomplex(10.0,10.0);

   return 0;
}

float _Complex fnt3(float _Complex *a, double _Complex *b) {

   if ( *a != createcomplexf(5.0f,5.0f) ) exit(39);
   if ( *b != createcomplex(10.0,10.0) ) exit(43);

   return 0;
}

float _Complex fnt4(float _Complex a, double _Complex b) {

   if ( a != createcomplexf(5.0f,5.0f) ) exit(47);
   if ( b != createcomplex(10.0,10.0) ) exit(51);

   return 0;
}

float _Complex fnt5(const float _Complex *a, const double _Complex *b) {

   if ( *a != createcomplexf(5.0f,5.0f) ) exit(55);
   if ( *b != createcomplex(10.0,10.0) ) exit(59);

   return 0;
}

float _Complex fnt6(const float _Complex a, const double _Complex b) {

   if ( a != createcomplexf(5.0f,5.0f) ) exit(63);
   if ( b != createcomplex(10.0,10.0) ) exit(67);

   return 0;
}
