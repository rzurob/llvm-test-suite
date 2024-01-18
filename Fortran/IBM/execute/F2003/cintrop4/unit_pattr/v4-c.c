  /**
  *** Fortran ENTRY in subroutine (one arg by value) called from C.
  **/

  void s1e1 (int);
  int main()
  {
    s1e1 (3);
  }

  /*
   *: Expected result:
   *   arg1 is  3
  */
