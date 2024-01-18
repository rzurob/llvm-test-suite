  /**
  *** Fortran function (one arg not-by-value) called from C.
  **/

  int f1 (int *);  /* prototype for Fortran function */
  int main()
  {
    int j = 3;

    /* call Fortran subroutine which expects arg passed NOT by value */
    printf ("f1 returned %d\n", f1 (&j));
  }

  /*
   * Expected result:
   *   f1 returned 4
  */
