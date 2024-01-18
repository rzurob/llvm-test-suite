!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: ffinal527.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/23/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : final sub (elemental final sub is used for
!                               arrays if there is no matching rank finalizer)
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

        final :: finalizeBase
    end type

    type dataType
        type (base), pointer :: data

        contains

        final :: finalizeData
    end type

    contains

    pure subroutine finalizeBase (b)
        type (base), intent(in) :: b

    end subroutine

    elemental subroutine finalizeData (d)
        type (dataType), intent(inout) :: d

        if (associated (d%data)) deallocate (d%data)
    end subroutine
end module

program ffinal527
use m
    type (dataType) :: d1(3)

    allocate (d1(1)%data, d1(2)%data, d1(3)%data)

    call abc (d1)

    contains

    subroutine abc (b)
        class (dataType), intent(out) :: b(:)

        do i = 1, size (b)
            if (associated (b(i)%data)) error stop 1_4
        end do
    end subroutine
end
