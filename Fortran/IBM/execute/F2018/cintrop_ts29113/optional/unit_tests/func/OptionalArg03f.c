/**********************************************************************
*  ===================================================================
*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
*  ===================================================================
*
*  TEST CASE TITLE            : OptionalArg03f
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
*                               where the procedure is defined in C. 
*                               - The actual argument is itself an optional
*                                 dummy argument that is not present
*
************************************************************************/

#include <stdio.h>

void c_func(int * a, double * b)
{
  if (a != NULL)
    printf("a = %d\n", *a);
  else
    printf("a = absent\n");

  if (b != NULL)
    printf("b = %.2f\n", *b);
  else
    printf("b = absent\n");

  return;
}
