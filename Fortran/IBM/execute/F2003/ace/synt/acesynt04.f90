!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : acesynt04
!*
!*  DATE                       : 2006-07-04
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : Use of square brackets replacing (//)
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : syntax, square brackets, array constructor
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  "(//)" is a legal FORMAT, but not a legal array constructor.  Various
!*  rewritings of this produce both legal formats and legal array constructors.
!*  Three of them are not correct formats, but the compiler lets them go with
!*  warnings.  We compile this test case and run it to verify that the formats
!*  work correctly, still.
!*
!*  A corresponding set of tests which should not work are given in acesynt04d.f
!*
!*  The test is successful if the output is correct.
!*
!* =============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890

program acesynt04
       integer i0, iarr(2)
       i0 = 88
       iarr = (/i0,10/)
       write(6,100)
       write(6,110) i0
       write(6,120) iarr
       write(6,130) iarr
100    format(//)
110    format(/i0/)
120    format(/(i0)/)
130    format((/(i0,i0=1,1)/))

140    f = format((/(i0)/))
       print *, f
150    f = format((/(i0,i0=1,1)/))
       print *, f
160    f = format([(i0)])
       print *, f
170    f = format([(i0,i0=1,1)])
       print *, f

contains
  real function format(arg)
    integer :: arg(*)
    format = arg(1) / 10.0
  end function format

end program acesynt04
