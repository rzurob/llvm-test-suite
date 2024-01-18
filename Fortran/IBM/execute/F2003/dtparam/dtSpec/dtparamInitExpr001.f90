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
!*  DATE                       : 02/17/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (type parameter inquiry)
!                               Case: inquiry on type parameter can be used as
!                               initialization expression.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type base (k)
        integer, kind :: k

        integer(k) :: id
    end type

    type(base(8)) b1(100)

    do i = 1, size(b1)
        call test(b1(i), 2_8**33+i)
    end do

    do i = 1, 100
        if (b1(i)%id - 2_8**33 /= i) error stop 1_4
    end do

    contains

    subroutine test (b, i)
        type(base(8)), intent(inout) :: b
        integer(b%k), intent(in) :: i

        b%id = i
    end subroutine
    end
