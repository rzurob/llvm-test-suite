!*  ===================================================================
!*
!*  DATE                       : 24/07/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND specifier= in OPEN statement
!*
!*  DESCRIPTION                :
!*          The round specifier has a limited list of character values.
!*          Any trailing blanks are ignored, but not blanks preceding.
!* ===================================================================

  program roundSpecifierOpen01d
    use ISO_FORTRAN_ENV

    real r, w
    r = 0.23

    open(11, file="tst11.dat", round='U '//'P')  !<--illegal

    open(12, file="tst12.dat", round='  D'//'OWN') !<--illegal

    open(13, file="tst13.dat", round='U'//'P  ')  !<--OK

    write(OUTPUT_UNIT, *, round=' U'//'P')  w !<--illegal

    read(INPUT_UNIT, *, round='  U'//'P')  r !<--illegal

    write(OUTPUT_UNIT, *, round='U'//'  P')  w !<--illegal

  end program roundSpecifierOpen01d
