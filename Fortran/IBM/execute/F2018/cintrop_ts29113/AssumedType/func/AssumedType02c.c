/**********************************************************************
*  ===================================================================
*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
*  ===================================================================
*
*  TEST CASE TITLE            : AssumedType01f
*
*  PROGRAMMER                 : Dorra Bouchiha 
*  DATE                       : June 13, 2012
*  ORIGIN                     : AIX Compiler Development,
*                             : IBM Software Solutions Toronto Lab
*
*  PRIMARY FUNCTIONS TESTED   : C-interop: Assumed Type objects
*
*  SECONDARY FUNCTIONS TESTED : None
*
*  DRIVER STANZA              : xlc
*  REQUIRED COMPILER OPTIONS  :
*
*  DESCRIPTION                : Calling a BIND(C) procedure from Fortran
*                               where the procedure is defined in C
*                               This test case focuses on C types that 
*                               are interoperable with INTEGER
*   
***********************************************************************
1234567890123456789012345678901234567890123456789012345678901234567890*/

#include <stdio.h>

void c_sub(void* a, void* b, void* c)
{
      *(int*)a = 5;
      *(float *)b = 2.0;
      *(char *)c = 'Z';

  return;
}
