#include "pf.h"  /* for pf() (Print and Flush) and macro str() */

#ifndef CTYPE
#  define CTYPE double
#endif
#ifndef CFORMAT
#  define CFORMAT %.17f
#endif

extern CTYPE f1_fB ();

/*
** int-function-no-args implemented in C:
*/
CTYPE
f1_cB ()
{
  pf ("%s entered\n", __FUNCTION__);
  return 3;
}


void
f1c_via_c ()
{
  pf ("\nC calling f1_cB\n");
  pf (" rv = " str(CFORMAT) "\n", f1_cB ());
}

void
f1f_via_c ()
{
  pf ("\nC calling f1_fB\n");
  pf (" rv = " str(CFORMAT) "\n", f1_fB ());
}
