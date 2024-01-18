#include <stdio.h>
#include <stdlib.h>

extern char LO, LO1[10], LO2[10][10], lOgical[2][2][3];


void csub()
{ 
  
  int i, j, k;
  
  if ( LO ==  0)
     exit(60);

  
  for(i=0;i<=1;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        if ( lOgical[i][j][k] == 0)
          exit(61);

  for (i=0; i< 10; i++)
     for (j=0; j< 10; j++)
       if ( LO2[i][j] == 0 )
          exit(62);

  for (i=0; i< 10; i++)
    if ( LO1[i] == 0)
       exit(63);
       
  fsub();
  
  if ( LO !=  0)
     exit(64);

  
  for(i=0;i<=1;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        if ( lOgical[i][j][k] != 0)
          exit(65);

  for (i=0; i< 10; i++)
     for (j=0; j< 10; j++)
       if ( LO2[i][j] != 0 )
          exit(66);

  for (i=0; i< 10; i++)
    if ( LO1[i] != 0)
       exit(67);

  LO = 1;
 
  for(i=0;i<=1;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        lOgical[i][j][k] = 1;

  for (i=0; i<=9; i++)
   for (j=0; j<= 9; j++)
     LO2[i][j] = 1;
  
  for (i=0; i<=9; i++)
    LO1[i] = 1;
  return;
}
