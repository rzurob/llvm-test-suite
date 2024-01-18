! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=self /tstdev/F2003/allocEnh/argAssociation/dummyArg009a.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=none

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
!*  DATE                       : 10/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               This is a simple test to verify that type-bound
!                               assignment applies for a derived type component
!                               that is nonpointer, nonallocatable and with a
!                               type-bound defined assignment.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)      :: data

        contains

        procedure :: uncommonAssgn
        generic :: assignment(=) => uncommonAssgn
    end type

    contains

    subroutine uncommonAssgn (b1, b2)
        class(base(*,4)), intent(inout) :: b1
        type(base(*,4)), intent(in) :: b2

        b1%data = b1%data + b2%data
    end subroutine
end module

module m1
use m
    type container(k2,n2)    ! (4,20)
        integer, kind     :: k2
        integer, len      :: n2
        type(base(n2,k2))    data
    end type
end module

program dummyArg009a
use m1
    type(container(4,20)) :: co1, co4
    logical(4), external :: precision_r4

    co1%data%data = 10.1
    co4%data%data = 20.2

    co1 = co4

    if (.not. precision_r4 (co1%data%data, 10.1_4+20.2_4)) error stop 1_4
    if (.not. precision_r4 (co4%data%data, 20.2_4)) error stop 2_4
end
