#include <stdio.h>
#include <math.h>
#include "cmplx.h"

void extsub_int(signed char *i, signed short *j, signed int *k, 
                signed long long *l, signed long long *m);

void extsub_real(float *i, double *j, long double *l);

void extsub_char(char *j, char *i);

void extsub_log(unsigned char *j, unsigned char *i );

void extsub_comp(float _Complex *a, double _Complex *b,
                 float _Complex *c, double _Complex *d);

int testf(float x, float y)
{   
   if (fabs((x - y)/(x + y)) > 0.0001f) return 1; 
     else return 0 ;
}

int testd(double x, double y)
{   
   if ( fabs((x - y)/(x + y)) > 0.000001 ) return 1; 
    else return 0;
}

int testl(long double x, long double y)
{
   if (fabs(x - y) > 0.000000000001l ) return 1; 
     else return 0;
}

int main()
{


   signed char ai1 = 5, bi1 = 5;
   signed short ai2 = 15, bi2 = 15;
   signed int ai4 = 11, bi4 = 11;
   signed long long ai8 = 17, bi8 = 17, i81 = 0;
 
   float   ar4 = 4.80f, br4 = 4.80f;
   double   ar8 = 40.0, br8 = 40.0;
   double  ar16 = 600.0, br16 = 600.0, r161 = 0.0;

   unsigned char al1 = 0, ll1 = 1;
  

   float _Complex  ac8 , bc8; 
   double _Complex  ac16 , bc16; 
   ac8 = createcomplexf(0.0f, 0.0f);
   bc8 = createcomplexf(1.0f, 1.0f);
   ac16 = createcomplex(0.0, 0.0);
   bc16 = createcomplex(1.0, 1.0);
   
   char ach1 = 'a', bch1 = 'd';
   
   extsub_int(&ai1, &ai2, &ai4, &ai8, &i81);
   
   if (ai1 != bi1) exit(10);
   if (ai2 != bi2) exit(11);
   if (ai4 != bi4) exit(12);
   if (ai8 != bi8) exit(13);
   if (i81 != 48) exit(14);
   
   
   extsub_real(&ar4, &ar8, &r161);
   if ( testf( ar4, br4 )) exit(20);
   if ( testd( ar8, br8 )) exit(21);
   if ( testd(r161, 44.8)) exit(23);
   
   extsub_log(&al1, &ll1);
   
   if ( al1 != 0 ) exit(30);
   if ( ll1 != 0 ) exit(31);
   
   extsub_char(&ach1, &bch1);
   
   if (ach1 != 'a' ) exit(50);
   if (bch1 != 'a' ) exit(51);

   extsub_comp(&ac8, &ac16, &bc8, &bc16);
   
   if ( testf(crealf(ac8), crealf(bc8) ) || 
        testf(cimagf(ac8), cimagf(bc8) )  ) exit(61);
        
    if ( testd(creal(ac16), creal(bc16) ) || 
        testd(cimag(ac16), cimag(bc16) )  ) exit(62);
   
}

