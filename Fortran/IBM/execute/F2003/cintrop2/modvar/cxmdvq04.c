#include <stdio.h>
#include <stdlib.h>
#include "cmplx.h"
#include <math.h>

extern float _Complex a, a1[10], a2[10][10], a3[1][2][3];
extern float _Complex b, bb,b1[10], b2[10][10], b3[1][2][3];
extern double _Complex c, cc, c1[10], c2[10][10], c3[1][2][3];
extern long double _Complex d, dd, d1[10], d2[10][10], d3[1][2][3];

extern void fsub();

int testf(float _Complex x)
{   
   if (fabs(crealf(x)) > 0.0001f) return 1; 
   if (fabs(cimagf(x) - 1.0f) > 0.0001f) return 1;
   return 0;
}

int testd(double _Complex x)
{  
   if ( fabs(creal(x)) > 0.000000000001 ) return 1;
   if ( fabs(cimag(x) - 1.0) > 0.000000000001) return 1;
   return 0;
}

int testl(long double _Complex x)
{
   if (fabs(creall(x)) > 0.000000000001l ) return 1; 
   if (fabs(cimagl(x) - 1.0l) > 0.000000000001) return 1;
   return 0;
}

void csub()
{ 
  int i, j, k;
  
  if ( testf(a) && testf(b) && testd(c) && testl(d) && testf(bb)  
       && testd(cc) && testl(dd) )
      exit(60);
      
  for ( i = 0; i < 10; i++)
     if ( testf(a1[i]) &&  testf(b1[i]) &&  testd(c1[i]) &&  testl(d1[i])  )
        exit(61);
        
  for ( i = 0; i <  10; i++ )
    for (j = 0; j < 10; j++)
     if ( testf(a2[i][j]) && testf(b2[i][j]) && testd(c2[i][j]) && 
            testl(d2[i][j])  )
         exit(62);
         
  for ( i = 0; i < 2; i++)
     for (j = 0; j < 3; j++)
        if ( testf(a3[0][i][j]) && testf(b3[0][i][j]) && 
              testd(c3[0][i][j]) && testl(d3[0][i][j]) )
              exit (63);
              
  fsub();
  
  if ( testf(createcomplexf(crealf(a) -2.0f, cimagf(a) - 1.0f)) 
       && testf(createcomplexf(crealf(b) -2.0f, cimagf(b) - 1.0f)) 
       && testd(createcomplex(creal(c) - 2.0, cimag(c) - 1.0) )
       && testl(createcomplexl(creall(d) - 2.0, cimagl(d) - 1.0l)) 
       && testf(createcomplexf(crealf(bb) -2.0f, cimagf(bb) - 1.0f))  
       && testd(createcomplex(creal(cc) - 2.0, cimag(cc) - 1.0) )
       && testl(createcomplexl(creall(dd) - 2.0, cimagl(dd) - 1.0l)) )
      exit(64);
      
  for ( i = 0; i < 10; i++)
     if ( testf(createcomplexf(crealf(a1[i]) -2.0f, cimagf(a1[i]) - 1.0f)) 
          &&  testf(createcomplexf(crealf(b1[i]) -2.0f, cimagf(b1[i]) - 1.0f)) 
          &&  testd(createcomplex(creal(c1[i]) - 2.0, cimag(c1[i]) - 1.0)) 
          &&  testl(createcomplexl(creall(d1[i]) - 2.0, cimagl(d1[i]) - 1.0l)) )
        exit(65);
        
  for ( i = 0; i <  10; i++ )
    for (j = 0; j < 10; j++)
     if ( testf(createcomplexf(crealf(a2[i][j]) -2.0f, cimagf(a2[i][j]) - 1.0f)) 
          &&  testf(createcomplexf(crealf(b2[i][j]) -2.0f, cimagf(b2[i][j]) - 1.0f)) 
          &&  testd(createcomplex(creal(c2[i][j]) - 2.0, cimag(c2[i][j]) - 1.0)) 
          &&  testl(createcomplexl(creall(d2[i][j]) - 2.0, cimagl(d2[i][j]) - 1.0l)) )
         exit(66);
         
  for ( i = 0; i < 2; i++)
     for (j = 0; j < 3; j++)
        if ( testf(createcomplexf(crealf(a3[0][i][j]) -2.0f, cimagf(a3[0][i][j]) - 1.0f)) 
          &&  testf(createcomplexf(crealf(b3[0][i][j]) -2.0f, cimagf(b3[0][i][j]) - 1.0f)) 
          &&  testd(createcomplex(creal(c3[0][i][j]) - 2.0, cimag(c3[0][i][j]) - 1.0)) 
          &&  testl(createcomplexl(creall(d3[0][i][j]) - 2.0, cimagl(d3[0][i][j]) - 1.0l)) )
              exit (67);

  a = createcomplexf(1.0f,3.0f);
  b = createcomplexf(1.0f,3.0f);
  c = createcomplex(1.0,3.0);
  d = createcomplexl(1.0l,3.0l);
  
  bb = createcomplexf(1.0f,3.0f);
  cc = createcomplex(1.0,3.0);
  dd = createcomplexl(1.0l,3.0l);
  
  for ( i = 0; i < 10; i++)
  {   a1[i] = createcomplexf(1.0f,3.0f);
      b1[i] = createcomplexf(1.0f,3.0f);
      c1[i] = createcomplex(1.0,3.0);
      d1[i] = createcomplexl(1.0l,3.0);
   }
            
  for ( i = 0; i <  10; i++ )
  {   
      for (j = 0; j < 10; j++)
      { 
          a2[i][j] = createcomplexf(1.0f,3.0f);
          b2[i][j] = createcomplexf(1.0f,3.0f);
          c2[i][j] = createcomplex(1.0,3.0);
          d2[i][j] = createcomplexl(1.0l,3.0l); 
       }
   }
         
  for ( i = 0; i < 2; i++)
  {  
      for (j = 0; j < 3; j++)
      {  
           a3[0][i][j] = createcomplexf(1.0f,3.0f);
           b3[0][i][j] = createcomplexf(1.0f,3.0f);
           c3[0][i][j] = createcomplex(1.0,3.0);
           d3[0][i][j] = createcomplexl(1.0l,3.0l);
       }
   }



  return;
}
