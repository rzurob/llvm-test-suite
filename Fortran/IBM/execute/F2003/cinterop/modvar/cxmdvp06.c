#include <stdlib.h>
#include <stdio.h>
#include "cmplx.h"
#include <math.h>


extern long double _Complex a, aa, a1[10], a2[10][10], a3[1][2][3];
extern void fsub();


int testl(long double _Complex x)
{
   if (fabs(creall(x)) > 0.00000000000000000000000000001l ) return 1; 
   if (fabs(cimagl(x) - 1.0l) > 0.00000000000000000000000000001l) return 1;
   return 0;
}

int main()
{ 
  int i, j, k;
  
   a = createcomplexl(1.0l, 3.0l);
  
  aa = createcomplexl(1.0l, 3.0l);
  
  for ( i = 0; i < 10; i++)
  {   a1[i] = createcomplexl(1.0l, 3.0l);
      
   }
            
  for ( i = 0; i <  10; i++ )
  {   
      for (j = 0; j < 10; j++)
      { 
          a2[i][j] = createcomplexl(1.0l, 3.0l);
           
       }
   }
         
  for ( i = 0; i < 2; i++)
  {  
      for (j = 0; j < 3; j++)
      {  
           a3[0][i][j] = createcomplexl(1.0l, 3.0l);
           
       }
   }

  fsub();
  
  if (  testl(a) && testl(aa) )
      exit(60);
      
  for ( i = 0; i < 10; i++)
     if (  testl(a1[i])  )
        exit(61);
        
  for ( i = 0; i <  10; i++ )
    for (j = 0; j < 10; j++)
     if ( testl(a2[i][j])  )
         exit(62);
         
  for ( i = 0; i < 2; i++)
     for (j = 0; j < 3; j++)
        if (  testl(a3[0][i][j]) )
              exit (63);

  
 

  return 0;
}
