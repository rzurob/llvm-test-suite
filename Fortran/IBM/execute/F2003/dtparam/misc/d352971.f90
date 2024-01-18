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
!*  DATE                       : 20090129
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : defect 352971
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m
    type X (n)
        integer, len :: n

        real data(n)
    end type

    type, extends(X) :: Y (l)
        integer, len :: l

        character(l) :: name
    end type
end module

use m
    implicit none
    type(Y(10, 20)), target :: c1(2,2)

    type(Y(:,:)), pointer :: p(:)

    logical(4), external :: precision_r4
    integer i

    p => c1(2,:)

    if (size(p) /= 2) stop 1
    if ((p%n /= 10) .or. (p%l /= 20)) stop 2

    p(1)%data = 1
    p(2)%data = 2
    p(1)%name = 'xlftest 101 xlftest 101'
    p(2)%name = 'testxlf 1001 testxlf 1001'

    do i = 1, 10
        if (.not. precision_r4(c1(2,1)%data(i), 1.0_4)) stop 3

        if (.not. precision_r4(c1(2,2)%data(i), 2.0_4)) stop 4
    end do

    if (c1(2,1)%name /= 'xlftest 101 xlftest ') stop 5

    if (c1(2,2)%name /= 'testxlf 1001 testxlf') stop 6

    print *, c1(2,:)
end
