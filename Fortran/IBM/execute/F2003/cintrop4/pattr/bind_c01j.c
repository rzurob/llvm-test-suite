#include <stdio.h>
#include <math.h>
#include "cmplx.h"

void sub_int(signed char *i, signed short *j, signed int *k, signed long long *l);

void sub_real(float *i, double *j);

void sub_char(char *i);

void sub_log(unsigned char *i );

void sub_comp(float _Complex *a, double _Complex *b);

int testf(float x, float y)
{   
   if ( fabs((2 * (x - y))/(x + y)) > 0.0001f) return 1; 
     else return 0 ;
}

int testd(double x, double y)
{  
   if ( fabs((2*(x - y))/(x + y)) > 0.0000000001 ) return 1; 
    else return 0;
}

int testl(long double x, long double y)
{
   if ( (2*fabs(x - y))/(x + y) > 0.000000000001l ) return 1; 
     else return 0;
}

int main()
{


   signed char ai1 = 5, bi1 = 8;
   signed short ai2 = 15, bi2 = 18;
   signed int ai4 = 11, bi4 = 14;
   signed long long ai8 = 17, bi8 = 20;
 
   float   ar4 = 4.80, br4 = 9.6;
   double   ar8 = 140.8, br8 = 281.6;

   unsigned char al1 = 0;
  

   float _Complex  ac8 , bc8; 
   double _Complex  ac16 , bc16; 
   ac8 = createcomplexf(0.0f, 0.0f);
   bc8 = createcomplexf(1.0f, 1.0f);
   ac16 = createcomplex(0.0, 0.0);
   bc16 = createcomplex(1.0, 1.0);
   
   char ach1 = 'a', bch1 = 'd';
   
   sub_int(&ai1, &ai2, &ai4, &ai8);
   
   if (ai1 != bi1) exit(10);
   if (ai2 != bi2) exit(11);
   if (ai4 != bi4) exit(12);
   if (ai8 != bi8) exit(13);
   
   
   sub_real(&ar4, &ar8);
   if ( testf( ar4, br4 )) exit(20);
   if ( testd( ar8, br8 )) exit(21);
   
   sub_log(&al1);
   
   if ( al1 == 0 ) exit(30);
   
   sub_char(&ach1);
   
   if (ach1 != bch1) exit(50);
   
   sub_comp(&ac8, &ac16);
   
   if ( testf(crealf(ac8), crealf(bc8) ) || 
        testf(cimagf(ac8), cimagf(bc8) )  ) exit(61);
        
    if ( testd(creal(ac16), creal(bc16) ) || 
        testd(cimag(ac16), cimag(bc16) )  ) exit(62);
   
}

