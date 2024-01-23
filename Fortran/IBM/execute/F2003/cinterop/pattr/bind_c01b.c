#include <stdio.h>
#include "cmplx.h"
#include <stdlib.h>
#include <math.h>
#include <string.h>

  struct der_bind
  {
   
   char ch1;
   unsigned char l1;

   signed char      i1;
   signed short     i2;
   signed int       i4;
   signed long long i8;

   float            r4;
   double           r8;
   

   float _Complex   x8;
   double _Complex  x16;
};



 void extsub_der(struct der_bind *a) {
   a->i1 = a->i1 + 3;
   a->i2 = a->i2 + 3;
   a->i4 = a->i4 + 3;
   a->i8 = a->i8 + 3;
  
   a->r4 = a->r4 * 2;
   a->r8 = a->r8 * 2;
  
 
   a->ch1 = 'd';

   a->l1 = 1;
   
   a->x8 = createcomplexf(6.0f, 8.0f);
   a->x16 = createcomplex(10.0, 12.0);
  }


