!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal513.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (very basic test for intent(out)
!*                               finalization)
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

        contains

        final :: finalizeBase
    end type

    contains

    subroutine finalizeBase (b)
        type (base), intent(inout) :: b

        if (associated(b%data)) deallocate (b%data)
    end subroutine
end module

use m
    type (base):: b1

    allocate (b1%data)

    call abc (b1)

    if (associated (b1%data)) error stop 1_4

    allocate (b1%data)

    call cba (b1)

    if (associated (b1%data)) error stop 2_4

    contains

    subroutine abc (b)
        type (base), intent(out) :: b
    end subroutine

    subroutine cba (b)
        class (base), intent(out) :: b
    end subroutine
end
