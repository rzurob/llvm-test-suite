#include <stdio.h>
#include <stdlib.h>

extern char lo, lo1[10], lo2[10][10], lo3[2][2][3];
extern void fsub();

int main()
{ 
  
  int i, j, k;
  
  lo = 0;
 
  for(i=0;i<=1;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        lo3[i][j][k] = 0;

  for (i=0; i<=9; i++)
   for (j=0; j<= 9; j++)
     lo2[i][j] = 0;
  
  for (i=0; i<=9; i++)
    lo1[i] = 0;
  
  
  fsub();
  
  if ( lo ==  0)
     exit(60);

  
  for(i=0;i<=1;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        if ( lo3[i][j][k] == 0)
          exit(61);

  for (i=0; i< 10; i++)
     for (j=0; j< 10; j++)
       if ( lo2[i][j] == 0 )
          exit(62);

  for (i=0; i< 10; i++)
    if ( lo1[i] == 0)
       exit(63);

  return 0;
}
