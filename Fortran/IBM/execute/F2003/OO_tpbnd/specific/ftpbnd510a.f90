!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ftpbnd510a.f
! %VERIFY: ftpbnd510a.out:ftpbnd510a.vf
! %STDIN:
! %STDOUT: ftpbnd510a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (invoke type bound through
!*                               a named constant and part of it)
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
        integer*4 :: id

        contains

        procedure, nopass :: typeID => baseID

        procedure, non_overridable :: getID => getBaseID

        procedure :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    integer*4 function baseID ()
        baseID = 1
    end function

    integer*4 function getBaseID (b)
        class (base), intent(in) :: b

        getBaseID = b%id
    end function
end module

module m1
use m, only : base
    type, extends(base) :: child
        class (base), pointer :: data => null()

        contains

        procedure, nopass :: typeID => childID
    end type

    contains

    subroutine increaseBase (b, i)
        class (base), intent(inout) :: b
        integer*4, intent(in) :: i

        b%id = b%id + i
    end subroutine

    integer*4 function childID ()
        childID = 2
    end function
end module

program ftpbnd510a
use m1
    type(child), parameter :: c = child(10)

    call c%base%print

    if (c%typeID() /= 2) error stop 1_4

    if (c%getID() /= 10) error stop 2_4

    if (c%base%typeID() /= 1) error stop 3_4

    if (c%base%getID () /= 10) error stop 4_4
end
