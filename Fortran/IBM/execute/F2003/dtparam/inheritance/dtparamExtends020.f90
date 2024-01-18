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
!*  DATE                       : 11/30/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: type parameters' names can be kind and
!                               len.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (kind, len)       !<-- user likely to use these two names
        integer, kind ::kind
        integer, len :: len

        integer(kind) id
        character(len) name
    end type

    type, extends(base) :: child (k2)
        integer, kind :: k2 = 4

        real(k2) data(len)
    end type

    type (base(kind=8, len=20)) b1_m
    type (child(len=30, k2=8, kind=8)) c1_m(10)
end module

program dtparamExtends020
use m
    type(child(4, 25)) c1
    logical(4) precision_r4, precision_r8

    b1_m%id = 100_8
    b1_m%name = 'xlftest 101'

    c1_m%id = (/(i, i=1, 10)/)
    c1_m%name = (/'c1_m 1', 'c1_m 2', 'c1_m 3', 'c1_m 4', 'c1_m 5', &
                'c1_m 6', 'c1_m 7', 'c1_m 8', 'c1_m 9', 'c1_m 0'/)

    do i = 1, 10
        c1_m(i)%data = (/((i-1)*1.0d2+j, j=1,30)/)
    end do

    c1%id = b1_m%id + 10
    c1%name = 'c1'
    c1%data = (/(i*1.1e0, i = 1, 25)/)


    !! verify the results
    print *, b1_m

    write (*, 100) c1_m
    write (*, 100) c1

100 format (i4, 1x, a, '.', 30f8.1)
end
