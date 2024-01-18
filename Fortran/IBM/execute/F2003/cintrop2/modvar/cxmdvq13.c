#include <stdio.h>
#include <math.h>
#include <stdlib.h>

extern float d, A, a2[10], a3[10][10], a1[1][2][3];
extern float c, B, A2[10], A1[10][10], A3[1][2][3];
extern double a, C, CCC[10], CcC[10][10], cCC[1][2][3];
extern long double b, D, CCc[10], Ccc[10][10], cCc[1][2][3];

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

void csub()
{ 
  int i, j, k;
  
  if ( testf(d) && testf(c) && testd(a) && testl(b) && testf(B)  
       && testd(C) && testl(D) && testf(A) )
      exit(60);
      
  for ( i = 0; i < 10; i++)
     if (testf(a2[i]) &&  testf(A2[i]) &&  testd(CCC[i]) &&  testl(CCc[i]-1.0l))
        exit(61);
        
  for ( i = 0; i <  10; i++ )
    for (j = 0; j < 10; j++)
     if ( testf(a3[i][j]) && testf(A1[i][j]) && testd(CcC[i][j]) && 
            testl(Ccc[i][j] - 1.0l)  )
         exit(62);
         
  for ( i = 0; i < 2; i++)
     for (j = 0; j < 3; j++)
        if ( testf(a1[0][i][j]) && testf(A3[0][i][j]) && 
              testd(cCC[0][i][j]) && testl(cCc[0][i][j] - 1.0l) )
              exit (63);

  fsub();
  
   if ( testf(d - 1.0f) && testf(c - 1.0f) && 
        testd(a - 1.0) && testl(b) && 
        testf(B - 1.0f) && testd(C - 1.0) && 
        testl(D - 1.0l) && testf(A - 1.0f) )
      exit(64);
      
  for ( i = 0; i < 10; i++)
     if (testf(a2[i] - 1.0f) &&  testf(A2[i] - 1.0f) &&  
         testd(CCC[i] - 1.0) &&  testl(CCc[i]-2.0l))
        exit(65);
        
  for ( i = 0; i <  10; i++ )
    for (j = 0; j < 10; j++)
     if ( testf(a3[i][j] - 1.0f) && testf(A1[i][j] - 1.0f) && 
          testd(CcC[i][j] - 1.0) && testl(Ccc[i][j] - 2.0l))
         exit(62);
         
  for ( i = 0; i < 2; i++)
     for (j = 0; j < 3; j++)
        if ( testf(a1[0][i][j] - 1.0f) && testf(A3[0][i][j] - 1.0f) && 
              testd(cCC[0][i][j] - 1.0) && testl(cCc[0][i][j] - 2.0l) )
              exit (63);


  d = 2.0f;
  c = 2.0f;
  a = 2.0;
  b = 2.0l;
  
  A = 2.0f;
  B = 2.0f;
  C = 2.0;
  D = 2.0l;
  
  for ( i = 0; i < 10; i++)
  {   a2[i] = 2.0f;
      A2[i] = 2.0f;
      CCC[i] = 2.0;
      CCc[i] = 3.0l;
   }
            
  for ( i = 0; i <  10; i++ )
  {   
      for (j = 0; j < 10; j++)
      { 
          a3[i][j] = 2.0f;
          A1[i][j] = 2.0f;
          CcC[i][j] = 2.0;
          Ccc[i][j] = 3.0l; 
       }
   }
         
  for ( i = 0; i < 2; i++)
  {  
      for (j = 0; j < 3; j++)
      {  
           a1[0][i][j] = 2.0f;
           A3[0][i][j] = 2.0f;
           cCC[0][i][j] = 2.0;
           cCc[0][i][j] = 3.0l;
       }
   }



  return;
}
