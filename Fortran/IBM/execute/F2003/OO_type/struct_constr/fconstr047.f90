!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr047.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/13/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (used for component with
!*                               default initialization)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        integer*4 :: i1 = 0
    end type
end module

module m1
use m
    type B
        type (A) :: a1 = A(i1 = 1)
    end type
end module

program fconstr047
use m1
    type (B) :: b1
    type (B) :: b2 = B (a1 = A(i1 = 10))

    type (A) :: a1

    type (A) :: a2 = A (i1 = 100)

    if (a1%i1 /= 0) error stop 1_4

    if (a2%i1 /= 100) error stop 2_4

    if (b1%a1%i1 /= 1) error stop 3_4

    if (b2%a1%i1 /= 10) error stop 4_4
end
