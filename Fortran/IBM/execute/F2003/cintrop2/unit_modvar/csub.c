#include <stdio.h>
#include <stdlib.h>
/*#include <complex.h>*/

/* Variable ch, a, z have been initialized in Fortran.*/
extern char ch[1];
/*extern float _Complex cm;*/
extern float cf;
extern int ci, ca[1][2][3];

void csub()
{ int i, j, k;
  
  printf("Display variables initialized in Fortran.\n");
  printf("In csub():\n ch=%s\n", ch);
/*  printf("cm=%f+i*%f\n", creal(cm), cimag(cm));  */
  printf("cf=%f\n", cf);
  printf("ci=%d\n", ci);
  for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        printf("ca[%d][%d][%d]=%d\n", i, j, k, ca[i][j][k]);

  /* Variables are changed in C */ 
  ch[0]='C';
/*  creal(cm) = 3.4;
  cimag(cm) = 1.2;*/
  cf+=1;
  ci+=1;

  for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        ca[i][j][k] += 1;

  printf("Display changed variables in C.\n");
  printf("In csub(), after changing:\nch=%s\n", ch);
/*  printf("cm=%f+i*%f\n", creal(cm), cimag(cm));*/
  printf("cf=%f\n", cf);
  printf("ci=%d\n", ci);
  for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        printf("ca[%d][%d][%d]=%d\n", i, j, k, ca[i][j][k]);

  return;
}
