#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <stdlib.h>


extern long double a, aa, a1[10], a2[10][10], a3[1][2][3];

extern void fsub();

int testl(long double x)
{
   if (fabs(x) > 0.00000000000000000000000000001l ) return 1; 
     else return 0;
}

int main()
{ 
  int i, j, k;
  
    a = 2.0l;
  aa = 2.0l;
  
  
  for ( i = 0; i < 10; i++)
  {   a1[i] = 2.0l;
      
   }
            
  for ( i = 0; i <  10; i++ )
  {   
      for (j = 0; j < 10; j++)
      { 
          a2[i][j] = 2.0l;
       }
   }
         
  for ( i = 0; i < 2; i++)
  {  
      for (j = 0; j < 3; j++)
      {  
           a3[0][i][j] = 2.0l;
           
       }
   }

  fsub();
  
  if ( testl(a) && testl(aa) )
      exit(60);
      
  for ( i = 0; i < 10; i++)
     if ( testl(a1[i])  )
        exit(61);
        
  for ( i = 0; i <  10; i++ )
    for (j = 0; j < 10; j++)
     if (  testl(a2[i][j])  )
         exit(62);
         
  for ( i = 0; i < 2; i++)
     for (j = 0; j < 3; j++)
        if ( testl(a3[0][i][j]) )
              exit (63);



  return 0;
}
