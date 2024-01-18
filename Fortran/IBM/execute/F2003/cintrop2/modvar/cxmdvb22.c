/* C code for testcase "fxmdvb22.f"  */
#include <inttypes.h>
#include <stdio.h> 

/* Fortran subroutine */
extern fsub();

/* The variables are defined at Fortran program */
/* and declared in C program using extern keyword */
extern long long c2darray[4][5];

int main()
{
  long long d2darray[4][5];
  int i, j;

  /* Initialize the array c2darray and c2darray */
  
  for (i=0;i<4;i++)
    {
      for (j=0;j<5;j++)
	{ 
	  c2darray[i][j]=10*(i+1)+j;
	  d2darray[i][j]=10*(i+1)+j;
	}
    }

  /* call fortran subroutine */
  fsub();

  /*Verify the result of matrix passed by Fortran program. */
  for (i=0;i<4;i++)
    {
      for (j=0;j<5;j++)
	{
	  if (c2darray[i][j]!=d2darray[i][j]*2)
	    return(1);
	}
    }

  return 0;

}
