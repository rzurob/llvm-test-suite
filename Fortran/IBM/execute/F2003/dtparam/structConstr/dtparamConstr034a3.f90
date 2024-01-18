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
!*  DATE                       : 03/16/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Use of procedure target whose interface
!                               mismatches with pointer component is invalid;
!                               fix the problem.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
        procedure(real(k)), nopass, pointer :: p
    end type

    type(base(8, 100)) b1

    contains

    real(8) function getReal (i,n, p)
        integer, intent(in) :: n, i(n)
        procedure(real(8)) :: p

        getReal = p(sum(i))
    end function
end module

program dtparamConstr034a3
use m
    logical(4), external :: precision_r8

    b1 = base(8, 100)(0, getReal)

    call assgnB1

    !! verify b1
    do i = 1, 100
        if (.not. precision_r8 (b1%data(i), real((i+1)*i/2, 8))) error stop 1_4
    end do
end


subroutine assgnB1
use m
    double precision, external :: int2Double

    do i = 1, 100
        b1%data(i) = b1%p((/(j, j=1,i)/), i, int2Double)
    end do
end subroutine


real(8) function int2Double (i)
    integer, intent(in) :: i

    int2Double = i*1.0d0
end function
