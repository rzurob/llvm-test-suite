! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (very basic test for intent(out)
!*                               finalization; followed by default
!*                               initialization)
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
        integer*4, pointer :: data
        integer*4 :: id = 0

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        b%id = -1
        if (associated(b%data)) deallocate (b%data)
    end subroutine
end module

program ffinal513a
use m
    type (base):: b1

    allocate (b1%data)
    b1%id = 100

    call abc (b1)

    if (associated (b1%data)) error stop 1_4
    if (b1%id /= 0) error stop 2_4

    allocate (b1%data)
    b1%id = 10

    call cba (b1)

    if (associated (b1%data)) error stop 3_4
    if (b1%id /= 0) error stop 4_4

    contains

    subroutine abc (b)
        type (base), intent(out) :: b
    end subroutine

    subroutine cba (b)
        class (base), intent(out) :: b
    end subroutine
end