
/*
        C code for testcase "fxison04.f" and "fxison05.f"
*/

#include <stdio.h>
#include <stdlib.h>
#ifdef CMPLX
   #include <complex.h>
#else
   #include "cmplx.h"
#endif

void sub1(float _Complex *a, double _Complex *b) {

#ifdef CMPLX
   if ( *a != 5.0f+I*5.0f ) exit(21);
#else
   if ( *a != createcomplexf(5.0f,5.0f) ) exit(23);
#endif
#ifdef CMPLX
   if ( *b != 10.0+I*10.0 ) exit(25);
#else
   if ( *b != createcomplex(10.0,10.0) ) exit(27);
#endif

#ifdef CMPLX
   *a = *a + 5.0f+I*5.0f;
#else
   *a = *a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   *b = *b + 10.0+I*10.0;
#else
   *b = *b + createcomplex(10.0,10.0);
#endif

}

void sub2(float _Complex a, double _Complex b) {

#ifdef CMPLX
   if ( a != 5.0f+I*5.0f ) exit(29);
#else
   if ( a != createcomplexf(5.0f,5.0f) ) exit(31);
#endif
#ifdef CMPLX
   if ( b != 10.0+I*10.0 ) exit(33);
#else
   if ( b != createcomplex(10.0,10.0) ) exit(35);
#endif

#ifdef CMPLX
   a = a + 5.0f+I*5.0f;
#else
   a = a + createcomplexf(5.0f,5.0f);
#endif
#ifdef CMPLX
   b = b + 10.0+I*10.0;
#else
   b = b + createcomplex(10.0,10.0);
#endif

}

void sub3(float _Complex *a, double _Complex *b) {

#ifdef CMPLX
   if ( *a != 5.0f+I*5.0f ) exit(37);
#else
   if ( *a != createcomplexf(5.0f,5.0f) ) exit(39);
#endif
#ifdef CMPLX
   if ( *b != 10.0+I*10.0 ) exit(41);
#else
   if ( *b != createcomplex(10.0,10.0) ) exit(43);
#endif

}

void sub4(float _Complex a, double _Complex b) {

#ifdef CMPLX
   if ( a != 5.0f+I*5.0f ) exit(45);
#else
   if ( a != createcomplexf(5.0f,5.0f) ) exit(47);
#endif
#ifdef CMPLX
   if ( b != 10.0+I*10.0 ) exit(49);
#else
   if ( b != createcomplex(10.0,10.0) ) exit(51);
#endif

}

void sub5(const float _Complex *a, const double _Complex *b) {

#ifdef CMPLX
   if ( *a != 5.0f+I*5.0f ) exit(53);
#else
   if ( *a != createcomplexf(5.0f,5.0f) ) exit(55);
#endif
#ifdef CMPLX
   if ( *b != 10.0+I*10.0 ) exit(57);
#else
   if ( *b != createcomplex(10.0,10.0) ) exit(59);
#endif

}

void sub6(const float _Complex a, const double _Complex b) {

#ifdef CMPLX
   if ( a != 5.0f+I*5.0f ) exit(61);
#else
   if ( a != createcomplexf(5.0f,5.0f) ) exit(63);
#endif
#ifdef CMPLX
   if ( b != 10.0+I*10.0 ) exit(65);
#else
   if ( b != createcomplex(10.0,10.0) ) exit(67);
#endif

}
