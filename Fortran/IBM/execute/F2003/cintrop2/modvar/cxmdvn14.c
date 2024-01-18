#include <stdio.h>
#include <stdlib.h>

extern char cha1[1], chaR[1], CH3[1], CHa4[5], ch3[1][2][3], cha4[2][2];
extern char ch1[1], CHar[1];


void csub()
{ 
  
  int i, j, k;
  
  if ( cha1[0] != 'a' || chaR[0] != 'b' || CH3[0] != 'c')
     exit(60);
  if ( ch1[0] != 'g' || CHar[0] != 'h' )
     exit(64);
  
  for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        if ( ch3[i][j][k] != 'e')
          exit(61);

  for (i=0; i<= 1; i++)
     for (j=0; j<=1; j++)
       if ( cha4[i][j] != 'f')
          exit(62);

  for (i=0; i<=4; i++)
    if ( CHa4[i] != 'd')
       exit(63);

  cha1[0] += 1;
  chaR[0] += 1;
  CH3[0] += 1;
  ch1[0] += 1;
  CHar[0] += 1;
 
  for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        ch3[i][j][k] += 1;

  for (i=0; i<=1; i++)
   for (j=0; j<= 1; j++)
     cha4[i][j] += 1;
  
  for (i=0; i<=4; i++)
    CHa4[i] += 1;
  return;
}
