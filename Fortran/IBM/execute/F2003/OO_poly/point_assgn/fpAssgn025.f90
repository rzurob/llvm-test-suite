!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn025.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (pointer object can be
!*                               structure component)
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
        integer*4 :: id = 0
    end type

    type, extends(base) :: child
        character*15 :: name = ''
    end type

    type (child), target, save :: c1_m
end module

module m1
use m
    type baseContainer
        class (base), pointer :: data => null()
    end type

    type anyContainer
        class (*), pointer :: data => null()
    end type

end module

program fpAssgn025
use m1
    interface assignment (=)
        subroutine base2Unlimited (ac, bc)
        use m1
            type (anyContainer), intent(out) :: ac
            type (baseContainer), intent(in) :: bc
        end subroutine
    end interface
    type (anyContainer) :: co_a1
    type (baseContainer) :: co_b1

    type (child), target :: c1

    co_b1 = baseContainer (data = c1_m)

    co_a1 = co_b1

    if (.not. associated (co_a1%data, c1_m)) error stop 1_4

    co_b1%data => c1
    co_a1%data => co_b1%data

    if (.not. associated (co_a1%data, c1)) error stop 2_4
end

subroutine base2Unlimited (ac, bc)
use m1
    type (anyContainer), intent(out) :: ac
    type (baseContainer), intent(in) :: bc

    ac%data => bc%data
end subroutine
