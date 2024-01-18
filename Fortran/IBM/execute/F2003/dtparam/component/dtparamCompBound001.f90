!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/18/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Bounds of derived type component can be
!                               named constant's type parameters: use kind type
!                               parameters first. (test C442)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (lb, ub)
        integer, kind :: lb, ub

        integer :: i1(lb:ub) = 1
    end type

    type(A(10, 100)), parameter :: a_const = A(10, 100)()

    type B
        real data (a_const%lb:a_const%ub)
    end type
end module

program dtparamCompBound001
use m
    type(B) b1

    if(size(b1%data) /= 91) error stop 1_4
end
