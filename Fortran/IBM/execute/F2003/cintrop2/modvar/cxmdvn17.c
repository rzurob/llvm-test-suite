#include <stdio.h>
#include <stdlib.h>
#include <math.h>

extern long double l16, a, A1[10], a1[10][10], A[1][2][3];



int testl(long double x)
{
   if (fabs(x) > 0.00000000000000000000000000001l ) return 1; 
     else return 0;
}

void csub()
{ 
  int i, j, k;
  
  if ( testl(l16) && testl(a) )
      exit(60);
      
  for ( i = 0; i < 10; i++)
     if ( testl(A1[i])  )
        exit(61);
        
  for ( i = 0; i <  10; i++ )
    for (j = 0; j < 10; j++)
     if (  testl(a1[i][j])  )
         exit(62);
         
  for ( i = 0; i < 2; i++)
     for (j = 0; j < 3; j++)
        if ( testl(A[0][i][j]) )
              exit (63);

  l16 = 2.0l;
  a = 2.0l;
  
  
  for ( i = 0; i < 10; i++)
  {   A1[i] = 2.0l;
      
      
   }
            
  for ( i = 0; i <  10; i++ )
  {   
      for (j = 0; j < 10; j++)
      { 
          a1[i][j] = 2.0l;
       }
   }
         
  for ( i = 0; i < 2; i++)
  {  
      for (j = 0; j < 3; j++)
      {  
           A[0][i][j] = 2.0l;
           
       }
   }



  return;
}
