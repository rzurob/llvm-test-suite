/**********************************************************************
*  ===================================================================
*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
*  ===================================================================
*
*  TEST CASE TITLE            : OptionalArg08f
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
*                               - The actual arg is an optional dummy arg
*
************************************************************************/

#include <stdio.h>

void sub(int * a)
{
  if (a == NULL)
  {
    printf("Not present!\n");
  }
  else
    printf("%d, %d, %d\n", a[0], a[4], a[9]);
}
