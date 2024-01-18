#include "pf.h"  /* for pf() -- Print and Flush */

main ()
{
  pf ("\nC main()\n");

  f1f_via_f ();
  f1c_via_f ();
  f1f_via_c ();
  f1c_via_c ();
}
