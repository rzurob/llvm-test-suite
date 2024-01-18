! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/func_return/ffuncRet013d.f
! opt variations: -ql

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
!*  DATE                       : 05/11/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly function return (recursive keyword and
!                               poly function results; diagnostic case: default
!                               IO on poly-function results)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind :: k1
        integer(k1)   :: data = -1
    end type

    contains

    recursive class(base(8)) function test1 ()
        pointer test1

        allocate (test1)
    end function

    recursive class(base(8)) function test2 ()
        allocatable test2

        allocate (test2)
    end function
end module

program ffuncRet013d
use m
    print *, test1()    !<-- illegal
    print *, test2()    !<-- illegal
end
