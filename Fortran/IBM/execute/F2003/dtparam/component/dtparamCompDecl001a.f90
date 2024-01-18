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
!                               be initialization expression: from subobject of
!                               named constants. (C445)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (kk)
        integer, kind :: kk

        integer :: k
        real(kk) :: data
    end type

    type (A(8)), parameter :: a8_const = A(8)(8, 1.1d0)
    type (A(4)), parameter :: a4_const = A(4)(4, 1.0e0)

    type base
        integer :: id = -1
        type(A(a8_const%k)) :: a1 = a8_const
        class(A(a4_const%k)), allocatable :: a2(:)
    end type
end module

program dtparamCompDecl001a
use m
    type (base), allocatable :: b1(:)

    logical(4), external :: precision_r8, precision_r4

    allocate (b1(100))

    if (b1(10)%id /= -1) error stop 1_4

    if (.not. precision_r8(b1(100)%a1%data, 1.1d0))  error stop 2_4

    allocate (b1(2)%a2(10), source=a4_const)

    if (.not. precision_r4(b1(2)%a2(8)%data, 1.0e0)) error stop 3_4
end
