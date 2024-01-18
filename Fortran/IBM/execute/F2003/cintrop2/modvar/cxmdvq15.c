#include <stdio.h>
#include <stdlib.h>

extern char LO, lo[10], KK[10][10], kk[2][2][3], lo2[10][10];


void csub()
{ 
  
  int i, j, k;
  
  if ( LO ==  0)
     exit(60);

  
  for(i=0;i<=1;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        if ( kk[i][j][k] == 0)
          exit(61);

  for (i=0; i< 10; i++)
     for (j=0; j< 10; j++)
       if ( KK[i][j] == 0 || lo2[i][j] != 0)
          exit(62);

  for (i=0; i< 10; i++)
    if ( lo[i] == 0)
       exit(63);
  
  fsub();
  
  if ( LO !=  0)
     exit(64);

  
  for(i=0;i<=1;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        if ( kk[i][j][k] != 0)
          exit(65);


  for (i=0; i< 10; i++)
     for (j=0; j< 10; j++)
     { printf("%d %d\n", kk[i][j], lo2[i][j]);
       if ( KK[i][j] != 0 || lo2[i][j] == 0)
          exit(66);}

  for (i=0; i< 10; i++)
    if ( lo[i] != 0)
       exit(67);

  LO = 1;
 
  for(i=0;i<=1;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        kk[i][j][k] = 1;

  for (i=0; i<=9; i++)
   for (j=0; j<= 9; j++)
    { KK[i][j] = 1;
      lo2[i][j] = 0;
     }
  
  for (i=0; i<=9; i++)
    lo[i] = 1;
  return;
}
