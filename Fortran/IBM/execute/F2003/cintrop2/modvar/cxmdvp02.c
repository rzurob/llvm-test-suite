#include <stdio.h>
#include <stdlib.h>

extern char ch1[1], ch2[1], ch3[1], ch4[5], ch5[1][2][3], ch6[2][2];
extern void fsub();

int main()
{ 
  
  int i, j, k;
  
  ch1[0] = 'b';
  ch2[0] = 'c';
  ch3[0] = 'd';
 
  for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        ch5[i][j][k] = 'f';

  for (i=0; i<=1; i++)
   for (j=0; j<= 1; j++)
     ch6[i][j] = 'g';
  
  for (i=0; i<=4; i++)
    ch4[i] = 'e';
  
  fsub();
  
  if ( ch1[0] != 'a' || ch2[0] != 'b' || ch3[0] != 'c')
     exit(60);

  
  for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        if ( ch5[i][j][k] != 'e')
          exit(61);

  for (i=0; i<= 1; i++)
     for (j=0; j<=1; j++)
       if ( ch6[i][j] != 'f')
          exit(62);

  for (i=0; i<=4; i++)
    if ( ch4[i] != 'd')
       exit(63);
  return 0;
  
}
