#include <stdio.h>
#include <stdlib.h>

extern char cha1[1], chaR[1], CH3[1], CHa4[5], chaRaCter[1][2][3], cha6[2][2];

extern void fsub();

int main()
{ 
  
  int i, j, k;
  
  cha1[0] = 'b';
  chaR[0] = 'c';
  CH3[0]  = 'd';
 
  for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        chaRaCter[i][j][k] = 'f';

  for (i=0; i<=1; i++)
   for (j=0; j<= 1; j++)
     cha6[i][j] = 'g';
  
  for (i=0; i<=4; i++)
    CHa4[i] = 'e';
    
  fsub();
  
  if ( cha1[0] != 'a' || chaR[0] != 'b' || CH3[0] != 'c')
     exit(60);

  
  for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        if ( chaRaCter[i][j][k] != 'e')
          exit(61);

  for (i=0; i<= 1; i++)
     for (j=0; j<=1; j++)
       if ( cha6[i][j] != 'f')
          exit(62);

  for (i=0; i<=4; i++)
    if ( CHa4[i] != 'd')
       exit(63);

  
  return 0;
}