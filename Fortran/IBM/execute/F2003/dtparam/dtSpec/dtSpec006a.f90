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
!*  DATE                       : 02/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: C481 part 1: The assumed-type-param for
!                               pointer dummy-arg; use pointer array dummy-arg.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real :: data(n) = -1.0
    end type

    contains

    subroutine updateBase (b, dim, r1)
        type (base(*)), pointer, intent(out) :: b(:)
        integer, intent(in) :: dim
        real, intent(in) :: r1(dim, b%n)

        allocate (b(dim))

        do i = 1, dim
            b(i)%data = r1(i,:)
        end do
    end subroutine
end module

program dtSpec006a
use m
    real r1(10000)

    type (base(100)), pointer :: b1(:)

    logical(4), external :: precision_r4

    r1 = (/(i*1.0, i=1, 10000)/)

    call updateBase (b1, 20, r1)

    !! verify that b1 is allocated with correct data
    if (.not. associated(b1)) error stop 1_4

    do i = 1, 20
        do j = 1, 100
            k = 20*(j-1) + i

            if (.not. precision_r4(b1(i)%data(j), k*1.0)) error stop 2_4
        end do
    end do
end
