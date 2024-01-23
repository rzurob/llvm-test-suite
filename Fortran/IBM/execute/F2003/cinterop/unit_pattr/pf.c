#include <stdarg.h>
#include <stdio.h>

int
pf (char *fmt, ...)  /* Print and Flush */
  /*
  ** This function is needed because we must flush the C buffer after
  ** each C output statement, to ensure that output from C routines doesn't
  ** arrive out of sequence with respect to output from Fortran routines.
  **
  ** This function accepts printf()'s arguments,
  ** passes them on to printf() and then calls fflush().
  ** It returns printf()'s return value.
  **
  ** This only proved necessary when output is going to
  ** a file or pipe -- not when it's going to a terminal.
  ** Disabling the Fortran runtime's buffering
  ** (with XLFRTEOPTS=buffering=disable_preconn) had no effect on this problem.
  */
{
  va_list ap;  /* Args Pointer */
  int myrv;    /* my Return Value */

  va_start (ap, fmt);
    myrv = vprintf (fmt, ap);
    fflush (stdout);
  va_end (ap);

  return myrv;
}
