! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (pointer and target can
!*                               be structure component; use rank-one arrays as
!*                               structure components)
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

    type (child), target, save :: c1_m (2:11)
end module

module m1
use m
    type baseContainer
        class (base), pointer :: data(:) => null()
    end type

    type anyContainer
        class (*), pointer :: data(:) => null()
    end type

end module

program fpAssgn025a1
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

    class (child), allocatable, target :: c1(:)

    co_b1 = baseContainer (data = c1_m)

    co_a1 = co_b1

    if (.not. associated (co_a1%data, c1_m)) error stop 1_4

    if ((size (co_a1%data) /= 10) .or. (lbound(co_a1%data, 1) /= 2) .or. &
        (ubound(co_a1%data, 1) /= 11)) error stop 2_4

    allocate (c1(20))

    co_b1%data => c1
    co_a1%data => co_b1%data(::2)

    if (.not. associated (co_a1%data, c1(::2))) error stop 3_4

    if (size (co_a1%data) /= 10) error stop 4_4
end

subroutine base2Unlimited (ac, bc)
use m1
    type (anyContainer), intent(out) :: ac
    type (baseContainer), intent(in) :: bc

    ac%data => bc%data
end subroutine
