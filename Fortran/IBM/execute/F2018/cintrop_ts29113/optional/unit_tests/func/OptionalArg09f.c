/**********************************************************************
*  ===================================================================
*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
*  ===================================================================
*
*  TEST CASE TITLE            : OptionalArg09f
*
*  PROGRAMMER                 : Bardia Mahjour
*  DATE                       : May 23, 2012
*  ORIGIN                     : AIX Compiler Development,
*                             : IBM Software Solutions Toronto Lab
*
*  PRIMARY FUNCTIONS TESTED   : C-interop OPTIONAL argument
*                                                   
*  SECONDARY FUNCTIONS TESTED : None 
*
*  DRIVER STANZA              : xlc
*  REQUIRED COMPILER OPTIONS  : 
*
*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
*                               where the procedure is defined in C and 
*                               contiguity checking for copy-in/out is
*                               required.
*                               - The actual arg is a pointer
*
************************************************************************/

#include <stdio.h>

void sub(int * a, float * b, int * n)
{
  int i;
  printf("--------------------\n");
  if (a == NULL)
  {
    printf("A: Not present!\n");
  }
  else
  {
    printf("A: ");
    for(i = 0; i < *n; i++)
      printf("%d ", a[i]);
    printf("\n");
  }
  if (b == NULL)
  {
    printf("B: Not present!\n");
  }
  else
  {
    printf("B: ");
    for(i = 0; i < *n; i++)
      printf("%.1f ", b[i]);
    printf("\n");
  }
  printf("--------------------\n");
}
