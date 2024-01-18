#include <stdio.h>
#include "cmplx.h"
#include <stdlib.h>
#include <math.h>
#include <string.h>

  struct der_bind
  {
   char    ch1;
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

int testf(float x, float y)
{   
   if (fabs(x - y) > (x + y) * 0.0001f) return 1; 
     else return 0 ;
}

int testd(double x, double y)
{  
   if ( fabs(x - y) > (x + y) * 0.000000000001 ) return 1; 
    else return 0;
}

 void sub_der(struct der_bind *a);
 
 void ssub_der(struct der_bind *a);
 
 int main() {
   
   struct der_bind a, b,c;
   
   
   
   a.i1 = 5;
   a.i2 = 15;
   a.i4 = 11;
   a.i8 = 17;
   
   b.i1 = 8;
   b.i2 = 18;
   b.i4 = 14;
   b.i8 = 20;
   
   c.i1 = 5;
   c.i2 = 15;
   c.i4 = 11;
   c.i8 = 17;
  
   a.r4 = 4.80f;
   a.r8 = 140.8;
   
   b.r4 = 9.6f;
   b.r8 = 281.6;
   
   c.r4 = 4.80f;
   c.r8 = 140.8;
 
 
   a.ch1 = 'a';
   b.ch1 = 'd';
   c.ch1 = 'a';
   
   a.l1 = 0;
   b.l1 = 1;
   c.l1 = 0;


   
   a.x8 = createcomplexf(3.0f, 4.0f);
   a.x16 = createcomplex(5.0, 6.0);
   b.x8 = createcomplexf(6.0f, 8.0f);
   b.x16 = createcomplex(10.0, 12.0);
   c.x8 = createcomplexf(3.0f, 4.0f);
   c.x16 = createcomplex(5.0, 6.0);
   
   sub_der( &a );
   
   if ( a.i1 != b.i1) exit(10);
   if ( a.i2 != b.i2) exit(11);
   if ( a.i4 != b.i4) exit(12);
   if ( a.i8 != b.i8) exit(13);
   
   if ( testf(a.r4, b.r4) ) exit(14);
   if ( testd(a.r8, b.r8) ) exit(15);
   
   if ( a.ch1 != b.ch1 ) exit(17);
   
   if ( a.l1 == 0 )  exit(18);
   
   if ( testf( crealf(a.x8), crealf(b.x8) ) ||
        testf( cimagf(a.x8), cimagf(b.x8) ) ) exit(19);
        
   if ( testd( creal(a.x16), creal(b.x16) ) ||
        testd( cimag(a.x16), cimag(b.x16) )  ) exit(20);
        
   
   ssub_der( &a );
   
   if ( a.i1 != c.i1) exit(30);
   if ( a.i2 != c.i2) exit(21);
   if ( a.i4 != c.i4) exit(22);
   if ( a.i8 != c.i8) exit(23);
   
   if ( testf(a.r4, c.r4) ) exit(24);
   if ( testd(a.r8, c.r8) ) exit(25);
   
   if ( a.ch1 != c.ch1 ) exit(27);
   
   if ( a.l1 != 0 ) exit(28);
   
   if ( testf( crealf(a.x8), crealf(c.x8) ) ||
        testf( cimagf(a.x8), cimagf(c.x8) ) ) exit(29);
        
   if ( testd( creal(a.x16), creal(c.x16) ) ||
        testd( cimag(a.x16), cimag(c.x16) )  ) exit(26);
   
   return 0;
  }


