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
!*  DATE                       : 01/27/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.4: default init.)
!                               Case: Try the default initializations by the
!                               parameterized components along; involve
!                               allocation for the deferred length type
!                               parameter.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A (k)
        integer, kind :: k

        real(k) :: data = 1.0e0
    end type

    type B (n)
        integer, len :: n

        complex :: cx(n) = (1.0, 1.0)
    end type

    type base (k, n)
        integer, kind :: k
        integer, len :: n

        type(A(k)) :: a1
        type(B(n)) :: b1(n)
    end type
end module

program dtparamCompInit005a
use m
    type (base(4, 10)) b1(10)
    type(base(8, 6)), allocatable :: b2(:)
    class(base(8, :)), pointer :: b3(:)

    logical(4), external :: precision_r4, precision_r8, precision_x8

    allocate (b2(10))
    allocate (base(8, 20) :: b3(25))

    !! verify the default initializations
    if (.not. precision_r4(b1(1)%a1%data, 1.0e0)) error stop 1_4

    do i = 1, 10
        if (.not. precision_x8(b1(i)%b1(i)%cx(i), cmplx(1.0, 1.0))) error stop 2_4
    end do

    if (.not. precision_r8(b2(5)%a1%data, real(1.0e0, 8))) error stop 3_4

    do i = 1, 6
        if (.not. precision_x8(b2(i)%b1(7-i)%cx(i), cmplx(1.0, 1.0))) error stop 4_4
    end do

    if (.not. precision_r8(b3(23)%a1%data, real(1.0e0, 8))) error stop 5_4

    do i = 1, 20
        if (.not. precision_x8(b3(25-i)%b1(i)%cx(i), cmplx(1.0, 1.0))) error stop 6_4
    end do
end
