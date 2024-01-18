#include <stdio.h>
#include <stdlib.h>

extern char ch1[1], ch2[1], ch3[1], ch4[5], ch5[1][2][3], ch6[2][2];


void csub()
{ 
  
  int i, j, k;
  
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
       
   fsub();
   
   if ( ch1[0] != 'b' || ch2[0] != 'c' || ch3[0] != 'd')
     exit(64);

  
  for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        if ( ch5[i][j][k] != 'f')
          exit(65);

  for (i=0; i<= 1; i++)
     for (j=0; j<=1; j++)
       if ( ch6[i][j] != 'g')
          exit(66);

  for (i=0; i<=4; i++)
    if ( ch4[i] != 'e')
       exit(67);

  ch1[0] += 1;
  ch2[0] += 1;
  ch3[0] += 1;
 
  for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        ch5[i][j][k] += 1;

  for (i=0; i<=1; i++)
   for (j=0; j<= 1; j++)
     ch6[i][j] += 1;
  
  for (i=0; i<=4; i++)
    ch4[i] += 1;
  return;
}
