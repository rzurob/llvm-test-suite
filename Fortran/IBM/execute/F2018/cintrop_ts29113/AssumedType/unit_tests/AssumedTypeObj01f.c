/**********************************************************************
*  ===================================================================
*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
*  ===================================================================
*
*  TEST CASE TITLE            : AssumedTypeObj01f
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
*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
*                               where the procedure is defined in C with
*                               assumed-type object.
*                               - Actual arg is of type intrinsic 
*
*1234567890123456789012345678901234567890123456789012345678901234567890*/

#include <stdio.h>

void c_func(void* a, int flag)
{
   if(flag == 1)
      printf("Type is c_short %hd\n",*(short*)a);
   else if(flag == 2)
      printf("Type is c_int %d\n",*(int*)a);
   else if(flag == 3)
      printf("Type is c_long %ld\n",*(long*)a);
   else if(flag == 4)
      printf("Type is c_long_long %lld\n",*(long long *)a);

   return;
}
