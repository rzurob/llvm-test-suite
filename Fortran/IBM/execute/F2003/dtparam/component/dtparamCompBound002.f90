!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/19/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Bounds of derived type component can be
!                               named constant's type parameters: use length
!                               type parameters.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (lb, ub)
        integer, len :: lb, ub

        integer(8) :: data (lb:ub) = 0
    end type

    type(A(10, 100)), parameter :: a_const = A(10,100)((/(i, i=10, 100)/))

    type B
        integer :: i (a_const%lb:a_const%ub)
    end type
end module

program dtparamCompBound002
use m
    type (B) :: b1

    b1 = B (a_const%data)

    if (any(b1%i /= (/(i, i=10, 100)/))) error stop 1_4
end
