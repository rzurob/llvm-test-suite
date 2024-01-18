
/*
        C code for testcase "fxison04.f" and "fxison05.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

void sub1(float _Complex *a, double _Complex *b) {

   if ( *a != createcomplexf(5.0f,5.0f) ) exit(23);
   if ( *b != createcomplex(10.0,10.0) ) exit(27);

   *a = *a + createcomplexf(5.0f,5.0f);
   *b = *b + createcomplex(10.0,10.0);

}

void sub2(float _Complex a, double _Complex b) {

   if ( a != createcomplexf(5.0f,5.0f) ) exit(31);
   if ( b != createcomplex(10.0,10.0) ) exit(35);

   a = a + createcomplexf(5.0f,5.0f);
   b = b + createcomplex(10.0,10.0);

}

void sub3(float _Complex *a, double _Complex *b) {

   if ( *a != createcomplexf(5.0f,5.0f) ) exit(39);
   if ( *b != createcomplex(10.0,10.0) ) exit(43);

}

void sub4(float _Complex a, double _Complex b) {

   if ( a != createcomplexf(5.0f,5.0f) ) exit(47);
   if ( b != createcomplex(10.0,10.0) ) exit(51);

}

void sub5(const float _Complex *a, const double _Complex *b) {

   if ( *a != createcomplexf(5.0f,5.0f) ) exit(55);
   if ( *b != createcomplex(10.0,10.0) ) exit(59);

}

void sub6(const float _Complex a, const double _Complex b) {

   if ( a != createcomplexf(5.0f,5.0f) ) exit(63);
   if ( b != createcomplex(10.0,10.0) ) exit(67);

}
