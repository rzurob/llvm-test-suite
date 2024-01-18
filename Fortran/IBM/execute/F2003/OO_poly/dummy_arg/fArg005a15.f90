! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (pointer return from
!*                               function is used as actual argument for
!*                               poly-pointer dummy-arg)
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

        procedure :: print => printBase
        procedure, non_overridable :: replicate => replicateBase
        final  :: finalizeBase
    end type

    type, extends(base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
        final  :: finalizeChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    function replicateBase (b)
        class (base), pointer :: replicateBase
        class (base), intent(in) :: b

        allocate (replicateBase, source=b)
    end function

    subroutine abc (b)
        class (base), pointer :: b

        if (associated (b)) then
            call b%print

            deallocate (b)
        end if
    end subroutine

    subroutine finalizeBase (b)
        type (base), intent(in) :: b

        print *, 'finalizeBase'
    end subroutine

    subroutine finalizeChild (c)
        type (child), intent(in) :: c

        print *, 'finalizeChild'
    end subroutine
end module

program fArg005a15
use m
    type (base) :: b1
    type (child) :: c1 = child(2, 'c1')

    b1%id = 1

    call abc (b1%replicate())

    call abc (c1%replicate())
end
