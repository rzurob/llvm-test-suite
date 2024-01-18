/**********************************************************************
*  ===================================================================
*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
*  ===================================================================
*
*  TEST CASE TITLE            : AssumedTypeObj03f
*
*  PROGRAMMER                 : Ren, Jian Gang
*  DATE                       : Apr 14, 2012
*  ORIGIN                     : Linux/AIX Compiler Development,
*                             : IBM Software Solutions China Lab
*
*  PRIMARY FUNCTIONS TESTED   : C-interop Assumed-type object
*                                                   
*  SECONDARY FUNCTIONS TESTED : None 
*
*  DRIVER STANZA              : xlc
*  REQUIRED COMPILER OPTIONS  : 
*
*
*  DESCRIPTION                : Calling a non-BIND(C) procedure from Fortran
*                               where the procedure is defined in C with
*                               assumed-type object.
*                               - Actual arg is an array 
*
*1234567890123456789012345678901234567890123456789012345678901234567890*/

#include <stdio.h>

void c_func_arr(void* a, int len)
{
  int i = 0;   

  int *b = (int *)a; 
  
  for (;i < len;i++)
  {
    printf("c_int arr: %d\n", b[i]);
  }

  return;
}
