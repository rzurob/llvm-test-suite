! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/17/2005
!*
!*  DESCRIPTION                : structure constructor (a nested allocatable
!                               component)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class (*), allocatable :: x
    end type

    integer, save :: counter = 0

    contains

    recursive subroutine printType (b)
        class (base), intent(in) :: b

        if (.not. allocated (b%x)) return

        select type (x =>b%x)
            type is (base)
                counter = counter + 1
                call printType (x)
            class default
                error stop 1_4
        end select
    end subroutine
end module

program fconstr504
use m
    type (base) :: b1
    b1 = base (x = base(x = base(x = base(null()))))

    call printType (b1)

    if (counter /= 3) error stop 5_4
end

