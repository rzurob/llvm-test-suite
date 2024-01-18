/**********************************************************************
*  ===================================================================
*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
*  ===================================================================
*
*  TEST CASE TITLE            : OptionalArg01f
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
*  DESCRIPTION                : Calling a BIND(C) procedure from C
*                               where the procedure is defined in
*                               Fortran.
*
************************************************************************/

#include <stdlib.h>

void sub(int * arg); // Defined in Fortran

int sub1(int *, int *);
int sub2(int *, int *);

int main(int argc, char ** argv)
{
  int ii = 5;
  int * jj;
  int kk = 11;
  int * const nptr = NULL;

  sub(NULL);
  sub(&ii);
  sub(nptr);

  jj = (int *) malloc (sizeof(int));
  *jj = 6;
  sub(jj);

  sub(NULL);

  free(jj);
  ii = 7;
  jj = &ii;
  sub(jj);
  
  jj = NULL;
  sub(jj);

  // Call Fortran subroutine through nested C calls.
  ii = sub1(NULL, &ii);
  sub1(&ii, NULL);
  sub1(NULL, NULL);
  sub1(&kk, &ii);

  return 0;
}

int sub1(int * dummy, int * arg)
{
  int local = sub2(dummy, arg);
  local--;
  return local;
}

int sub2(int * dummy, int * arg)
{
  int local = 0;

  sub(arg);

  if (dummy != NULL)
    return *dummy + local;
  else
    return -1;
}
