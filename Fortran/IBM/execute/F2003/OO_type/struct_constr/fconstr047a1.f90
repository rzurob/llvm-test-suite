!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr047a1.f
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
!*  DESCRIPTION                : structure constructor (for component with
!*                               partial default initialization)
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
        character*20 :: name = 'no-name'
        integer*4 :: id
    end type
end module

module m1
use m
    type B
        type (A) :: a1 = A (id = 1)
    end type
end module

program fconstr047a1
use m1
    type (B) :: b1

    type (B) :: b2 = B (a1 = A (name = 'b2', id = 10))

    if ((b1%a1%name /= 'no-name') .or. (b1%a1%id /= 1)) error stop 1_4

    if ((b2%a1%name /= 'b2') .or. (b2%a1%id /= 10)) error stop 2_4

    b1 = B()

    if ((b1%a1%name /= 'no-name') .or. (b1%a1%id /= 1)) error stop 3_4

    b1 = B (A(id = -1))

    if ((b1%a1%name /= 'no-name') .or. (b1%a1%id /= -1)) error stop 4_4
end
