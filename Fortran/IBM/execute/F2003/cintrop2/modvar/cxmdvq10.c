#include <stdio.h>
#include <stdlib.h>
#include "cmplx.h"
#include <math.h>

extern float _Complex A, A1[10], A2[10][10], A3[1][2][3];
extern float _Complex B, BB,B1[10], b2[10][10], B3[1][2][3];
extern double _Complex c, Comp1ex, c1[10], C2[10][10], c3[1][2][3];
extern long double _Complex Co, Dd, d1[10], d2[10][10], D3[1][2][3];

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
  
  if ( testf(A) && testf(B) && testd(c) && testl(Co) && testf(BB)  
       && testd(Comp1ex) && testl(Dd) )
      exit(60);
      
  for ( i = 0; i < 10; i++)
     if ( testf(A1[i]) &&  testf(B1[i]) &&  testd(c1[i]) &&  testl(d1[i])  )
        exit(61);
        
  for ( i = 0; i <  10; i++ )
    for (j = 0; j < 10; j++)
     if ( testf(A2[i][j]) && testf(b2[i][j]) && testd(C2[i][j]) && 
            testl(d2[i][j])  )
         exit(62);
         
  for ( i = 0; i < 2; i++)
     for (j = 0; j < 3; j++)
        if ( testf(A3[0][i][j]) && testf(B3[0][i][j]) && 
              testd(c3[0][i][j]) && testl(D3[0][i][j]) )
              exit (63);
  
  fsub();
  
  if ( testf(createcomplexf(crealf(A) -2.0f, cimagf(A) - 1.0f)) 
       && testf(createcomplexf(crealf(B) -2.0f, cimagf(B) - 1.0f)) 
       && testd(createcomplex(creal(c) - 2.0, cimag(c) - 1.0) )
       && testl(createcomplexl(creall(Co) - 2.0, cimagl(Co) - 1.0l)) 
       && testf(createcomplexf(crealf(BB) -2.0f, cimagf(BB) - 1.0f))  
       && testd(createcomplex(creal(Comp1ex) - 2.0, cimag(Comp1ex) - 1.0) )
       && testl(createcomplexl(creall(Dd) - 2.0, cimagl(Dd) - 1.0l)))
      exit(64);
      
  for ( i = 0; i < 10; i++)
     if ( testf(createcomplexf(crealf(A1[i]) -2.0f, cimagf(A1[i]) - 1.0f)) 
          &&  testf(createcomplexf(crealf(B1[i]) -2.0f, cimagf(B1[i]) - 1.0f)) 
          &&  testd(createcomplex(creal(c1[i]) - 2.0, cimag(c1[i]) - 1.0)) 
          &&  testl(createcomplexl(creall(d1[i]) - 2.0, cimagl(d1[i]) - 1.0l)))
        exit(65);
        
  for ( i = 0; i <  10; i++ )
    for (j = 0; j < 10; j++)
     if ( testf(createcomplexf(crealf(A2[i][j]) -2.0f, cimagf(A2[i][j]) - 1.0f)) 
          &&  testf(createcomplexf(crealf(b2[i][j]) -2.0f, cimagf(b2[i][j]) - 1.0f)) 
          &&  testd(createcomplex(creal(C2[i][j]) - 2.0, cimag(C2[i][j]) - 1.0)) 
          &&  testl(createcomplexl(creall(d2[i][j]) - 2.0, cimagl(d2[i][j]) - 1.0l))  )
         exit(66);
         
  for ( i = 0; i < 2; i++)
     for (j = 0; j < 3; j++)
       if(testf(createcomplexf(crealf(A3[0][i][j]) -2.0f, cimagf(A3[0][i][j]) - 1.0f)) 
          &&  testf(createcomplexf(crealf(B3[0][i][j]) -2.0f, cimagf(B3[0][i][j]) - 1.0f)) 
          &&  testd(createcomplex(creal(c3[0][i][j]) - 2.0, cimag(c3[0][i][j]) - 1.0)) 
          &&  testl(createcomplexl(creall(D3[0][i][j]) - 2.0, cimagl(D3[0][i][j]) - 1.0l)) )
              exit (67);
  
  A = createcomplexf(1.0f,3.0f);
  B = createcomplexf(1.0f,3.0f);
  c = createcomplex(1.0,3.0);
  Co = createcomplexl(1.0l,3.0l);
 
  BB = createcomplexf(1.0f,3.0f);
  Comp1ex = createcomplex(1.0,3.0);
  Dd = createcomplexl(1.0l,3.0l);
 
  
  for ( i = 0; i < 10; i++)
  {   A1[i] = createcomplexf(1.0f,3.0f);
      B1[i] = createcomplexf(1.0f,3.0f);
      c1[i] = createcomplex(1.0,3.0);
      d1[i] = createcomplexl(1.0l,3.0);
   }
            
  for ( i = 0; i <  10; i++ )
  {   
      for (j = 0; j < 10; j++)
      {   A2[i][j] = createcomplexf(1.0f,3.0f);
          b2[i][j] = createcomplexf(1.0f,3.0f);
          C2[i][j] = createcomplex(1.0,3.0);
          d2[i][j] = createcomplexl(1.0l,3.0l); 
           
       }
   }
         
  for ( i = 0; i < 2; i++)
  {  
      for (j = 0; j < 3; j++)
      {    A3[0][i][j] = createcomplexf(1.0f,3.0f);
           B3[0][i][j] = createcomplexf(1.0f,3.0f);
           c3[0][i][j] = createcomplex(1.0,3.0);
           D3[0][i][j] = createcomplexl(1.0l,3.0l);
          
       }
   }



  return;
}
