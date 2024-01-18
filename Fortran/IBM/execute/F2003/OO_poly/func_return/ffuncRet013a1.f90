!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/12/2005
!*
!*  DESCRIPTION                : poly-function results (recursive function
!                               return)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), private :: data

        contains

        procedure :: print => printBase
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        write (*, '(e15.5)') b%data
    end subroutine

    recursive class(base) function sum1 (n)
        allocatable sum1
        integer, intent(in) :: n

        real(8) :: seed = 1.0_8

        if (n <= 1) then
            seed = 1.0_8
            allocate (sum1, source=base(1.0_8))
        else
            associate (x => sum1 (n-1))
                seed = seed + 1.0_8

                allocate (sum1, source=base(seed+x%data))
            end associate
        end if
    end function
end module

program ffuncRet013a1
use m
    class(base), allocatable :: b1

    associate (x => sum1(1000))
        call x%print
    end associate

    allocate (b1, source=sum1(20000))

    call b1%print
end
