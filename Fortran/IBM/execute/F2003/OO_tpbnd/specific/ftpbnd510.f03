! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type bound (binding calls can be
!*                               invoked for parameter constant as long as the
!*                               dummy-arg is intent(in))
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

        procedure :: getID => getBaseID

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

program ftpbnd510
use m
    type(base), parameter :: b = base(10)

    call b%print

    if (b%typeID() /= 1) error stop 1_4

    if (b%getID() /= 10) error stop 2_4
end
