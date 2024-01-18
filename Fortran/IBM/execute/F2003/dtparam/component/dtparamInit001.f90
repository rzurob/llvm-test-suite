!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/19/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: component)
!                               Case: Initializations of type parameters: type
!                               parameter inquiry of a named constant is an
!                               initialization expression.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    integer, parameter :: a_constLB = 0
    integer, parameter :: a_constUB = 100

    type, private :: boundFinder (lb, ub)
        integer, kind :: lb = a_constLB
        integer, len :: ub = a_constUB
    end type

    type (boundFinder), parameter :: a_const = boundFinder()

    type B (lb, ub)
        integer, len :: lb = a_constlb, ub = a_constub

        real, dimension(lb:ub) :: data
    end type
end module

program dtparamInit001
use m
    type (B) b1(2)
    logical(4), external :: precision_r4

    if ((a_const%lb /= 0) .or. (a_const%ub /= 100)) stop 100

    b1(1)%data = (/(i*1.2e0, i=1, 101)/)

    b1(2) = b1(1)

    do i = 0, 100
        if (.not. precision_r4(b1(2)%data(i), (i+1)*1.2e0)) error stop 1_4
    end do
end
