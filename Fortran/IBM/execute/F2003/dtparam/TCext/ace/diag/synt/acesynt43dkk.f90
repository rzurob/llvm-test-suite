!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : acesynt43dkk
!*
!*                               by David Forster)
!*  DATE                       : 2007-11-16 (original: 2006-11-03)
!*  ORIGIN                     : Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*                               (+ Array Constructor Enhancements)
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement statement
!*                               function invocation
!*
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*                               (original: Feature Number 289053)
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : statement function
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*  Statement functions do not allow arrays as arguments, so the use of an AC
!*  should be rejected.  (Yes, statement functions are deleted, but they're
!*  still supported and therefore should be tested.)
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acesynt43dmod

  implicit none
  type derived (kderived_1,kderived_2) ! kderived_1,kderived_2=1,4
     integer, kind :: kderived_1,kderived_2
     integer(kderived_2) :: val = kderived_2
     character(kderived_1) :: c = 'd'
  end type derived

end module acesynt43dmod


program acesynt43dkk

  use acesynt43dmod
  implicit none

  integer :: i, fun1, fun2, iarr(1)
  type (derived(1,4)) :: dta(1), dt, dfun ! tcx: (1,4)
  fun1(i)  = i / 10
  fun2(dt) = dt % val
  dfun(i)  = derived(1,4)(i) ! tcx: (1,4)

  ! Should produce no error messages:
  print *, fun1(900), fun2(dt), dfun(3)
  i    = fun2(dt)
  iarr = fun1(900)
  dt   = dfun(3)

  ! All of these are wrong:

  print *, fun1((/900/))
  iarr = fun2([dt])
  dt   = dfun([(1,i=1,1)])

end program acesynt43dkk


! Extensions to introduce derived type parameters:
! type: derived - added parameters (kderived_1,kderived_2) to invoke with (1,4)/declare with (1,4) - 2 changes
