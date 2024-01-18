/**********************************************************************
*  ===================================================================
*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
*  ===================================================================
*
*  TEST CASE TITLE            : AssumedTypeObj04f
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
*
*1234567890123456789012345678901234567890123456789012345678901234567890*/

#include <stdio.h>

void c_func_add(void* a, void* b)
{
  int v1 = *(int *)a;
  int v2 = *(int *)b; 
  
  printf("(int)a + b: %d\n", v1 + v2);
  
  return;
}

void c_func_arr_sum(void* a, void* b, void* len)
{
  int i = 0;   
  int ln;
  int res = 0;

  int *arr1 = (int *)a; 
  int *arr2 = (int *)b;
  
  ln = *(int *)len;

  for (;i < ln;i++)
  { 
    res += (arr1[i] + arr2[i]);
  }
  printf("Result is : %d\n", res);
  return;
}
