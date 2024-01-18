  /**
  *** Fortran ENTRY in function (no args) called from C, both declared BIND(C).
  **/

  int f1 (int);
  int f1e1 (void);
  int main()
  {
    printf ("%d\n", f1 (3));
    printf ("%d\n", f1e1 ());
  }

  /*
   * Expected result:
   *   4
   *   1
  */
