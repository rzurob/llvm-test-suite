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
!*  DATE                       : 04/30/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               structure constructor (zero-sized array in the
!                               structure component)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n)
        integer, len :: n

        complex(4) :: data(n)
    end type
end module

program dtparamConstr059
use m
    type(base(:)), allocatable :: b2

    logical(4), external :: precision_x8

    b2 = base(0)([complex(4):: ])

    if (.not. allocated(b2)) error stop 1_4

    if (b2%n /= 0) error stop 2_4

    b2 = base(2) ([complex(4) :: 1, 2])

    if ((.not. allocated(b2)) .or. (b2%n /= 2)) error stop 3_4

    if ((.not. precision_x8 (b2%data(1), cmplx(1))) .or. &
        (.not. precision_x8 (b2%data(2), cmplx(2)))) error stop 4_4

    b2 = base(0)([complex(8):: ])        !<-- this ICE

    if (.not. allocated(b2)) error stop 5_4
    if (size(b2%data) /= 0) error stop 6_4
end

