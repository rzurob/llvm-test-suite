/* C code for testcase "fxmdvc12.f"  */
#include <inttypes.h>
#include <stdio.h> 

/* Fortran subroutine */
extern fsub();

extern int_least8_t c3darray[4][5][6];
int main()
{
  int_least8_t d3darray[4][5][6];
  int i, j,k;
  
  /* Initialize the array value */
  for (i=0;i<4;i++)
    {
      for (j=0;j<5;j++)
	{
	  for (k=0;k<6;k++)
            { 
	      c3darray[i][j][k]=3*(i+1)+2*(j+1)+k;
	      d3darray[i][j][k]=3*(i+1)+2*(j+1)+k;
            }
	}   
    }
  /* Call Fortran Subroutine  */
  fsub();

  /*Verify the result of matrix passed by Fortran program. */
  for (i=0;i<4;i++)
    {
      for (j=0;j<5;j++)
	{ 
	  for (k=0;k<6;k++)
            {
	      if (c3darray[i][j][k]!=d3darray[i][j][k]*2)
		return(1);
            }
	}
    }
  return 0;

}
