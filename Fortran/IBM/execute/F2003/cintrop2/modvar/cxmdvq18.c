#include <stdio.h>
#include <stdlib.h>
#include "cmplx.h"
#include <math.h>


extern long double _Complex A, a, a1[10], A1[10][10], A3[1][2][3];



int testl(long double _Complex x)
{
   if (fabs(creall(x)) > 0.00000000000000000000000000001l ) return 1; 
   if (fabs(cimagl(x) - 1.0l) > 0.00000000000000000000000000001l) return 1;
   return 0;
}

void csub()
{ 
  int i, j, k;
  
  if (  testl(a) && testl(A) )
      exit(60);
      
  for ( i = 0; i < 10; i++)
     if (  testl(a1[i])  )
        exit(61);
        
  for ( i = 0; i <  10; i++ )
    for (j = 0; j < 10; j++)
     if ( testl(A1[i][j])  )
         exit(62);
         
  for ( i = 0; i < 2; i++)
     for (j = 0; j < 3; j++)
        if (  testl(A3[0][i][j]) )
              exit (63);
  fsub();
  
  if (  testl(createcomplexl(creall(a) - 2.0l, cimagl(a))) 
      && testl(createcomplexl(creall(A) -2.0l, cimagl(A) - 1.0l)) )
      exit(64);
      
  for ( i = 0; i < 10; i++)
     if (  testl(createcomplexl(creall(a1[i]) -2.0l, cimagl(a1[i]) - 1.0l))  )
        exit(65);
        
  for ( i = 0; i <  10; i++ )
    for (j = 0; j < 10; j++)
     if ( testl(createcomplexl(creall(A1[i][j]) -2.0l, cimagl(A1[i][j]) - 1.0l))
      )
         exit(66);
         
  for ( i = 0; i < 2; i++)
     for (j = 0; j < 3; j++)
        if (  testl(createcomplexl(creall(A3[0][i][j]) -2.0l, cimagl(A3[0][i][j]) - 1.0l))
         )
              exit (67);
  
  
  
  a = createcomplexl(1.0l, 4.0l);
  
  A = createcomplexl(1.0l, 3.0l);
  
  for ( i = 0; i < 10; i++)
  {   
      a1[i] = createcomplexl(1.0l, 3.0l);
   }
            
  for ( i = 0; i <  10; i++ )
  {   
      for (j = 0; j < 10; j++)
      { 
          
          A1[i][j] = createcomplexl(1.0l, 3.0l);
       }
   }
         
  for ( i = 0; i < 2; i++)
  {  
      for (j = 0; j < 3; j++)
      {  
           
           A3[0][i][j] = createcomplexl(1.0l, 3.0l);
       }
   }



  return;
}