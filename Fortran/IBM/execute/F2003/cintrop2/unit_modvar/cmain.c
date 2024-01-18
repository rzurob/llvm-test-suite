#include <stdio.h>
#include <stdlib.h>
/*#include <complex.h>*/

/* Fortran subroutine */
extern fsub();

/* variables */
char ch[1];
/*float _Complex cm;*/
float cf;
int ci, ca[1][2][3];

main()
{
 int i, j, k;
 ch[0]='C';
 /* cm */
 cf = 1.0;
 ci = 2;
 for(i=0;i<=0;i++)
   for(j=0;j<=1;j++)
     for(k=0;k<=2;k++)
       ca[i][j][k]=3;

/* Initialize variables ch, cm, cf, ci, ca.*/
 printf("In C main program:\n");
 printf("ch=%s\n", ch);
/*  printf("cm=%f+i*%f\n", creal(cm), cimag(cm));  */
 printf("cf=%f\n", cf);
 printf("ci=%d\n", ci);
 for(i=0;i<=0;i++)
   for(j=0;j<=1;j++)
     for(k=0;k<=2;k++)
       printf("ca[%d][%d][%d]=%d\n", i, j, k, ca[i][j][k]);

 /*Call Fortran subroutine */
 fsub();

 /*variables as global entity are changed in Fortran. */
 printf("Return to C main program:\n");
 printf("ch=%s\n", ch);
/*  printf("cm=%f+i*%f\n", creal(cm), cimag(cm));*/
  printf("cf=%f\n", cf);
  printf("ci=%d\n", ci);
  for(i=0;i<=0;i++)
    for(j=0;j<=1;j++)
      for(k=0;k<=2;k++)
        printf("ca[%d][%d][%d]=%d\n", i, j, k, ca[i][j][k]);

  return 0;
}
