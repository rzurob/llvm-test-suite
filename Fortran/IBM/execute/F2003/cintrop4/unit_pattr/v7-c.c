  /**
  *** C function (one arg) called from Fortran via NAME= binding label
  *** containing mixed case and surrounded by blanks.
  **/

  int Func1 (int arg1)
  {
    printf ("hello\n");
    return ++arg1;
  }
