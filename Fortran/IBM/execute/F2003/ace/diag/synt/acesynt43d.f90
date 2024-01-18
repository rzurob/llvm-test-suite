!*******************************************************************************
!*  ============================================================================
!*  XL Fortran Test Case                                   IBM INTERNAL USE ONLY
!*  ============================================================================
!*
!*  TEST CASE NAME             : acesynt43d
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : David Forster
!*  DATE                       : 2006-11-03
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Array Constructor Enhancements
!*
!*  SECONDARY FUNCTIONS TESTED : statement function invocation
!*
!*  REFERENCE                  : Feature Number 289053
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : statement function
!*  TARGET(S)                  : 
!*  NUMBER OF TESTS CONDITIONS : 
!*
!*  DESCRIPTION
!*
!*  Statement functions do not allow arrays as arguments, so the use of an AC
!*  should be rejected.  (Yes, statement functions are deleted, but they're
!*  still supported and therefore should be tested.)
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

module acesynt43dmod

  implicit none
  type derived
     integer :: val = 4
     character :: c = 'd'
  end type derived

end module acesynt43dmod


program acesynt43d

  use acesynt43dmod
  implicit none

  integer :: i, fun1, fun2, iarr(1)
  type (derived) :: dta(1), dt, dfun
  fun1(i)  = i / 10
  fun2(dt) = dt % val
  dfun(i)  = derived(i)

  ! Should produce no error messages:
  print *, fun1(900), fun2(dt), dfun(3)
  i    = fun2(dt)
  iarr = fun1(900)
  dt   = dfun(3)

  ! All of these are wrong:

  print *, fun1((/900/))
  iarr = fun2([dt])
  dt   = dfun([(1,i=1,1)])

end program acesynt43d
