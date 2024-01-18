
/*
        C code for testcase "fxisoo04.f" and "fxisoo05.f"
*/

#include <stdio.h>
#include <stdlib.h>
   #include "cmplx.h"

void sub1(long double _Complex *a) {

   if ( *a != createcomplexl(5.0l,5.0l) ) exit(23);

   *a = *a + createcomplexl(5.0l,5.0l);

}

void sub2(long double _Complex a) {

   if ( a != createcomplexl(5.0l,5.0l) ) exit(27);

   a = a + createcomplexl(5.0l,5.0l);

}

void sub3(long double _Complex *a) {

   if ( *a != createcomplexl(5.0l,5.0l) ) exit(31);

}

void sub4(long double _Complex a) {

   if ( a != createcomplexl(5.0l,5.0l) ) exit(35);

}

void sub5(const long double _Complex *a) {

   if ( *a != createcomplexl(5.0l,5.0l) ) exit(39);

}

void sub6(const long double _Complex a) {

   if ( a != createcomplexl(5.0l,5.0l) ) exit(43);

}
