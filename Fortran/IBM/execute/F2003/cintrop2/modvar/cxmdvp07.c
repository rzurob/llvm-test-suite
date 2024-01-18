#include <stdio.h>
#include <stdlib.h>
#include <math.h>

extern float a, ab, a1[10], a2[10][10], a3[1][2][3];
extern float B, dA, b1[10], B2[10][10], b3[1][2][3];
extern double c, cc, c1[10], c2[10][10], c3[1][2][3];
extern long double D, DD, d1[10], d2[10][10], D3[1][2][3];

extern void fsub();

int testf(float x)
{   
   if (fabs(x) > 0.0001f) return 1; 
     else return 0 ;
}

int testd(double x)
{  
   if ( fabs(x) > 0.000000000001 ) return 1; 
    else return 0;
}

int testl(long double x)
{
   if (fabs(x) > 0.000000000001l ) return 1; 
     else return 0;
}

int main()
{ 
  int i, j, k;
  
  
  a = 2.0f;
  B = 2.0f;
  c = 2.0;
  D = 2.0l;
  
  ab = 2.0f;
  dA = 2.0f;
  cc = 2.0;
  DD = 2.0l;
  
  for ( i = 0; i < 10; i++)
  {   a1[i] = 2.0f;
      b1[i] = 2.0f;
      c1[i] = 2.0;
      d1[i] = 2.0l;
   }
            
  for ( i = 0; i <  10; i++ )
  {   
      for (j = 0; j < 10; j++)
      { 
          a2[i][j] = 2.0f;
          B2[i][j] = 2.0f;
          c2[i][j] = 2.0;
          d2[i][j] = 2.0l; 
       }
   }
         
  for ( i = 0; i < 2; i++)
  {  
      for (j = 0; j < 3; j++)
      {  
           a3[0][i][j] = 2.0f;
           b3[0][i][j] = 2.0f;
           c3[0][i][j] = 2.0;
           D3[0][i][j] = 2.0l;
       }
   }

  fsub();
  
  if ( testf(a) && testf(ab) && testf(B) && testd(c) && testl(D) && testf(dA)  
       && testd(cc) && testl(DD) )
      exit(60);
      
  for ( i = 0; i < 10; i++)
     if ( testf(a1[i]) &&  testf(b1[i]) &&  testd(c1[i]) &&  testl(d1[i])  )
        exit(61);
        
  for ( i = 0; i <  10; i++ )
    for (j = 0; j < 10; j++)
     if ( testf(a2[i][j]) && testf(B2[i][j]) && testd(c2[i][j]) && 
            testl(d2[i][j])  )
         exit(62);
         
  for ( i = 0; i < 2; i++)
     for (j = 0; j < 3; j++)
        if ( testf(a3[0][i][j]) && testf(b3[0][i][j]) && 
              testd(c3[0][i][j]) && testl(D3[0][i][j]) )
              exit (63);

  

  return 0;
}
