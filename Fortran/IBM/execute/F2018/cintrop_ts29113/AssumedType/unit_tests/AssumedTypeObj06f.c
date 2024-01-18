/**********************************************************************
*  ===================================================================
*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
*  ===================================================================
*
*  TEST CASE TITLE            : AssumedTypeObj06f
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
*                               - Actual arg is of polymorphic object. 
*
*1234567890123456789012345678901234567890123456789012345678901234567890*/

#include <stdio.h>

typedef struct dt
{
  int i;
}dt_t;

typedef struct et
{
  int i;
  int j;
}et_t;

typedef struct ft
{
  int i;
  int k;
}ft_t;

void c_func_dyn(void* a, void* flag)
{
  int f = *(int *)flag;

  if (f == 1)
  {
    dt_t *d = (struct dt *)a;
    printf("dt: i = %d\n", d->i);
  }
  else if (f == 2)
  {
    et_t *e = (struct et *)a;
    printf("et: i = %d\n", e->i);
    printf("et: j = %d\n", e->j);
  }
  else if (f == 3)
  {
    ft_t *f = (struct ft *)a;
    printf("ft: i = %d\n", f->i);
    printf("ft: k = %d\n", f->k);
  }
  else
  {
    printf("Error!");
  }
  return;
}

