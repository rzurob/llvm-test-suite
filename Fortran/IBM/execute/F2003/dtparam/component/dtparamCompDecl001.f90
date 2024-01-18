!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 01/19/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Type-parameter values for components can
!                               be initialization expression: from type
!                               parameter inquiry of named constants.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k)
        integer, kind :: k

        real(k) :: data
    end type

    type (A(8)), parameter :: a8_const = A(8)(1.0d0)
    type (A(4)), parameter :: a4_const = A(4)(1.1e0)

    type base
        integer :: id = -1
        type(A(a8_const%k)) :: a1(2) = a8_const
        class(A(a4_const%k)), allocatable :: a2
    end type
end module

program dtparamCompDecl001
use m
    type (base) b1

    logical(4), external :: precision_r8, precision_r4

    if (b1%id /= -1) error stop 1_4

    if ((.not. precision_r8(b1%a1(1)%data, 1.0d0)) .or. &
        (.not. precision_r8(b1%a1(2)%data, 1.0d0))) error stop 2_4

    allocate (b1%a2, source=a4_const)

    if (.not. precision_r4(b1%a2%data, 1.1e0)) error stop 3_4
end
