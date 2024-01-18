/* C code for testcase "fxbind_c03aae.f" */
/* Requires C compiler -qlongdbl option. */
#pragma options ldbl128
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

struct der_bind
{
  long double      r16;
  double           r8;
  float            r4;

  signed long long i8;
  signed int       i4;
  signed short     i2;
  signed char      i1;

  unsigned char    l1;
  char             ch1;
   
};

int fun_der_ref(struct der_bind *a) {
  a->i1 = a->i1 + 3;
  a->i2 = a->i2 + 3;
  a->i4 = a->i4 + 3;
  a->i8 = a->i8 + 3;
  
  a->r4 = a->r4 * 2;
  a->r8 = a->r8 * 2;
  a->r16 = a->r16 * 2;
  a->ch1 = 'd';
  return(0);
}

int fun_der_val(struct der_bind a) {
   
  a.i1 = a.i1 + 3;
  a.i2 = a.i2 + 3;
  a.i4 = a.i4 + 3;
  a.i8 = a.i8 + 3;
  
  a.r4 = a.r4 * 2;
  a.r8 = a.r8 * 2;
  a.r16 = a.r16 * 2;
 
  a.ch1 = 'd';

  return(0);
}
