!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/11/2005
!*
!*  DESCRIPTION                : poly function return (recursive function)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) :: data = 0
    end type

    contains

    recursive class(base) function factorial (n)
        allocatable factorial
        integer, intent(in) :: n

        integer(8) :: seed = 1

        if (n <= 1) then
            allocate (factorial, source=base(1))
            seed = 1
        else
            associate (x => factorial(n-1))
                seed = seed + 1
                allocate (factorial, source=base(seed*x%data))
            end associate
        end if
    end function
end module

program ffuncRet013a
use m
    class (base), allocatable :: b1

    allocate (b1, source=factorial(3))

    if (b1%data /= 6) error stop 1_4

    associate (x => factorial(10))
        if (x%data /= 3628800) error stop 2_4
    end associate

    select type (x => factorial(12))
        type is (base)
            if (x%data /= 479001600_8) error stop 3_4
        class default
            error stop 4_4
    end select
end
