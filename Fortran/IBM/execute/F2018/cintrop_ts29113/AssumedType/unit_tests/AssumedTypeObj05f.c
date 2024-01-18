/**********************************************************************
*  ===================================================================
*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
*  ===================================================================
*
*  TEST CASE TITLE            : AssumedTypeObj05f
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
*                               - Actual arg is of derived-type. 
*
*1234567890123456789012345678901234567890123456789012345678901234567890*/

#include <stdio.h>

typedef struct c_intr
{
  int a;
  short b;
  double c;  
}c_intr_t;

int c_func_print_str(void* a)
{
  c_intr_t *v = (struct c_intr *)a;

  printf("v->a: %d\n", v->a);
  printf("v->b: %d\n", v->b);
  printf("v->c: %g\n", v->c);
  
  return (v->a + v->b);
}
