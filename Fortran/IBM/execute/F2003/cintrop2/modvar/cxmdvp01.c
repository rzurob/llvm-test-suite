#include <stdio.h>
#include <math.h>
#include <stdlib.h>

extern float a;
extern float a1[10], a2[10][10], a3[1][2][3];
extern float b;
extern float bb,b1[10], b2[10][10], b3[1][2][3];
extern double c;
extern double cc, c1[10], c2[10][10], c3[1][2][3];
extern long double d;
extern long double dd, d1[10], d2[10][10], d3[1][2][3];

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
  b = 2.0f;
  c = 2.0;
  d = 2.0l;
  
  bb = 2.0f;
  cc = 2.0;
  dd = 2.0l;
  
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
          b2[i][j] = 2.0f;
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
           d3[0][i][j] = 2.0l;
       }
   }

  fsub();
  
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

  return 0;
 
}