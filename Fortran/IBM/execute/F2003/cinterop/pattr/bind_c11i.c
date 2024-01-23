#include <stdio.h>
#include <math.h>
#include "cmplx.h"

 signed char exfun_int1(signed char *i);

 short exfun_int2(signed short *j);

 int exfun_int4(signed int *k);

 signed long long exfun_int8(signed long long *l);

 float exfun_real4(float *i);

 double exfun_real8(double *j);

 unsigned char  exfun_log1(unsigned char *i);

 float _Complex exfun_comp1(float _Complex *c);
  
 double _Complex exfun_comp2(double _Complex *c);
 
 int testf(float x, float y)
{   
   if (fabs((x - y)/(x + y)) > 0.0001f) return 1; 
     else return 0 ;
}

int testd(double x, double y)
{  
   if ( fabs((x - y)/(x + y)) > 0.00000001 ) return 1; 
    else return 0;
}

int testl(long double x, long double y)
{
   if (fabs(x - y) > 0.000000000001l ) return 1; 
     else return 0;
}

int main()

{

   signed char ai1 = 5, bi1 = 8, ci1 = 5;
   signed short ai2 = 15, bi2 = 18, ci2 = 15;
   signed int ai4 = 11, bi4 = 14, ci4 = 11;
   signed long long ai8 = 17, bi8 = 20, ci8 = 17;
 
   float   ar4 = 4.80f, br4 = 9.6, cr4 = 4.80f;
   double   ar8 = 140.8, br8 = 281.6, cr8 = 140.8;

   unsigned char al1 = 0, bl1;
  

   float _Complex  ac8 , bc8, cc8; 
   double _Complex  ac16 , bc16, cc16;
   ac8 = createcomplexf(0.0f, 0.0f);
   bc8 = createcomplexf(1.0f, 1.0f);
   cc8 = createcomplexf(0.0f, 0.0f);
   ac16 = createcomplex(0.0, 0.0);
   bc16 = createcomplex(1.0, 1.0);
   cc16 = createcomplex(0.0, 0.0);
   
   char ach1 = 'a', bch1 = 'd', cch1 = 'a';

   ci1 = exfun_int1(&ai1);
     if ( ci1 != bi1 ) exit(10);
     if ( ai1 != 5 ) exit(110);
     
   ci2 = exfun_int2(&ai2);
     if ( ci2 != bi2 ) exit(11);
     if ( ai2 != 15 ) exit(111);

   ci4 = exfun_int4(&ai4);
     if (ci4 != bi4 ) exit(12);
     if (ai4 != 11 ) exit(112);

   ci8 = exfun_int8(&ai8);
     if (ci8 != bi8) exit(13);
     if (ai8 != 17 ) exit(113);
     
   cr4 = exfun_real4(&ar4);
     if (testf(cr4, br4)) exit(20);
     if (testf(ar4, 4.8f)) exit(120);
     
   cr8 = exfun_real8(&ar8);
     if (testd(cr8, br8)) exit(21);
     if (testd(ar8, 140.8)) exit(121);
     
   bl1 = exfun_log1(&al1);
     if (bl1 == 0) exit(30);
     if (al1 != 0) exit(130);
     
   cc8 = exfun_comp1(&ac8);
     if ( testf(crealf(cc8), crealf(bc8) ) || 
        testf(cimagf(cc8), cimagf(bc8) )  ) exit(61);
     if ( testf(crealf(ac8), 0.0f) || testf(cimagf(ac8), 0.0f) )
        exit(161);
        
   cc16 = exfun_comp2(&ac16);
      if ( testd(creal(cc16), creal(bc16) ) || 
        testd(cimag(cc16), cimag(bc16) )  ) exit(62);
      if ( testd(creal(ac16), 0.0) || testd(cimag(ac16), 0.0) )
         exit (162);

   return 0;
   
 }
   
