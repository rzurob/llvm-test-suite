#include <stdio.h>
#include <math.h>
#include "cmplx.h"

void extsub_int(signed char *i, signed short *j, signed int *k, signed long long *l);

void extsub_real(float *i, double *j);

void extsub_char(char *i);

void extsub_log(unsigned char *i );

void extsub_comp(float _Complex *a, double _Complex *b);

void sextsub_int(signed char *i, signed short *j, signed int *k, signed long long *l);

void sextsub_real(float *i, double *j);

void sextsub_char(char *i);

void sextsub_log(unsigned char *i );

void sextsub_comp(float _Complex *a, double _Complex *b);

int testf(float x, float y)
{   
   if ( (2*fabs(x - y)) > (x + y) * 0.0001f) return 1; 
     else return 0 ;
}

int testd(double x, double y)
{  
   if ( (2*fabs(x - y)) > (x + y) * 0.000000000001 ) return 1; 
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
   long double  ar16 = 1600.3, br16 = 3200.6, cr16 = 1600.3;

   unsigned char al1 = 0;
  

   float _Complex  ac8 , bc8, cc8; 
   double _Complex  ac16 , bc16, cc16;
   ac8 = createcomplexf(0.0f, 0.0f);
   bc8 = createcomplexf(1.0f, 1.0f);
   cc8 = createcomplexf(0.0f, 0.0f);
   ac16 = createcomplex(0.0, 0.0);
   bc16 = createcomplex(1.0, 1.0);
   cc16 = createcomplex(0.0, 0.0);
   
   char ach1 = 'a', bch1 = 'd', cch1 = 'a';
   
   extsub_int(&ai1, &ai2, &ai4, &ai8);
   
   if (ai1 != bi1) exit(10);
   if (ai2 != bi2) exit(11);
   if (ai4 != bi4) exit(12);
   if (ai8 != bi8) exit(13);
   
   extsub_real(&ar4, &ar8);
   
   if ( testf( ar4, br4 )) exit(20);
   if ( testd( ar8, br8 )) exit(21);
   
   extsub_log(&al1);
   
   if ( al1 == 0 ) exit(30);
   
   extsub_char(&ach1);
   
   if (ach1 != bch1) exit(50);
   
   extsub_comp(&ac8, &ac16);
   
   if ( testf(crealf(ac8), crealf(bc8) ) || 
        testf(cimagf(ac8), cimagf(bc8) )  ) exit(61);
        
    if ( testd(creal(ac16), creal(bc16) ) || 
        testd(cimag(ac16), cimag(bc16) )  ) exit(62);
        
        
   sextsub_int(&ai1, &ai2, &ai4, &ai8);
   
   if (ai1 != ci1) exit(14);
   if (ai2 != ci2) exit(15);
   if (ai4 != ci4) exit(16);
   if (ai8 != ci8) exit(17);
   
   
   sextsub_real(&ar4, &ar8);
   
   if ( testf( ar4, cr4 )) exit(23);
   if ( testd( ar8, cr8 )) exit(24);
   
   sextsub_log(&al1);
   
   if ( al1 != 0 ) exit(31);
   
   sextsub_char(&ach1);
   
   if (ach1 != cch1) exit(51);
   
   sextsub_comp(&ac8, &ac16);
   
   if ( testf(crealf(ac8), crealf(cc8) ) || 
        testf(cimagf(ac8), cimagf(cc8) )  ) exit(63);
        
    if ( testf(creal(ac16), creal(cc16) ) || 
        testf(cimag(ac16), cimag(cc16) )  ) exit(64);
  
}

