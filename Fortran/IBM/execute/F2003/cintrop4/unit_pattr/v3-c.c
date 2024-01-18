  /**
  *** Interoperable Fortran subroutine referenced from Fortran as well as C.
  **/

  void s2 (void);  /* prototype for Fortran subroutine */
  void s3 (void)
  {
    s2();  /* call Fortran subroutine */
  }
