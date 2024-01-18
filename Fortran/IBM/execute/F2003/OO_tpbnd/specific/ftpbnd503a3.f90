!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd503a3.f
! %VERIFY: ftpbnd503a3.out:ftpbnd503a3.vf
! %STDIN:
! %STDOUT: ftpbnd503a3.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (inherited binding called
!*                               in the overriding binding)
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
    type base
        integer*4, private :: id = 10

        contains

        procedure, non_overridable :: getID => baseID
        procedure :: print => printBase
    end type

    contains

    integer*4 function baseID (b)
        class (base), intent(in) :: b

        baseID = b%id
    end function

    subroutine printBase (c)
        class (base), intent(in) :: c

        print *, c%id
    end subroutine
end module

module m1
use m, only : base
    type, extends(base) :: child
        character*20 :: name = ''

        contains

        procedure :: print => printChild
    end type

    class (base), pointer :: b_ptr => null()
    contains

    subroutine printChild (c)
        class (child), intent(in) :: c

        print *, c%getID(), c%name
    end subroutine
end module

use m1
    type(child), target :: c1
    class (child), pointer :: c_ptr

    c1 = child (name = 'c1')

    call c1%print
    call c1%base%print

    if (c1%getID() /= 10) error stop 1_4

    c_ptr => c1
    b_ptr => c_ptr

    call b_ptr%print

    call c_ptr%print

    call c_ptr%base%print
end
