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
!*  DATE                       : 12/17/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam
!                               Case: test the intrinisc assignment involving
!                               derived type with scalar parameterized
!                               components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type realArray (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) value(n)
    end type

    type container (k, n)
        integer, kind :: k
        integer, len :: n

        type(realArray(k, n+1)), allocatable :: data
    end type
end module

program dtparamIntrinAssgn001
use m
    type (container (8, 10)) c1, c2
    type (container (4, 15)) c3(10), c4, c5

    !! test scalar in assignment
    allocate (c1%data)

    c1%data%value = (/(i*1.2d1, i=1, 11)/)

    c2 = c1

    write(*, '(11f12.2)') c2%data%value

    !! test arrays in assignment
    allocate (c4%data, c5%data)

    c4%data%value = (/(i*1.1e1+.5e0, i = 1, 16)/)
    c5%data%value = (/(i*2.e0, i=1,16)/)

    c3(1:2) = c4
    c3(3:4) = c5

    c3(7:10) = c3(4:1:-1)

    if (allocated(c3(5)%data) .or. allocated(c3(6)%data))   error stop 1_4

    do i = 1, 4
        write (*, '(16f12.2)') c3(i)%data
    end do

    do i = 7, 10
        write (*, '(16f12.2)') c3(i)%data
    end do
end
