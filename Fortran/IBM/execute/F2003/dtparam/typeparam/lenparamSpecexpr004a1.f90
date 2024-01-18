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
!*  DATE                       : 01/04/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Length type parameter may be used as
!                               specification expression in derived type
!                               definition: length type parameters for
!                               allocatable/pointer components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n = 10

        real :: data(n) = 0.0
    end type

    interface
        function genBase (n)
        import
            integer, intent(in) :: n
            type (base(n)) genBase
        end function
    end interface

    type A (k, l)
        integer, len :: l
        integer, kind :: k = 4

        integer(k) :: id
        type(base(abs(l))), pointer :: p1 => null()
        type(base(mod(abs(l), 5)+1)), allocatable :: p2(:)
    end type
end module

program lenparamSpecexpr004a1
use m
    type (A(4, -10)) a1
    type (A(8, 12)) a2(10)

    logical(4) precision_r4

    allocate(a1%p1, a1%p2(2), a2(10)%p1, a2(1)%p2(10))

    a1%id = 1

    a2%id = (/(i*2_8**32, i = 1, 10)/)

    a1%p1 = genBase(10)
    a2(10)%p1 = genBase(12)

    a1%p2(1) = genBase(1)
    a2(1)%p2(10) = genBase(3)


    !!verify results
    do i = 1, 10
        if (.not. precision_r4(a1%p1%data(i), i*1.0e0)) error stop 1_4
    end do

    do i = 1, 12
        if (.not. precision_r4(a2(10)%p1%data(i), i*1.0e0)) error stop 2_4
    end do

    if (.not. precision_r4(a1%p2(1)%data(1), 1.0_4)) error stop 3_4

    do i = 1, 3
        if (.not. precision_r4(a2(1)%p2(10)%data(i), i*1.0_4)) error stop 4_4
    end do
end

function genBase(n)
use m, only:base
    integer, intent(in) :: n
    type(base(n)) genBase

    genBase%data = (/(i*1.0e0, i=1,n)/)
end function
