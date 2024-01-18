! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn001d5.f
! opt variations: -qck -ql

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
!*  DATE                       : 04/28/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : data pointer assignment (use of object without
!                               pointer attribute in places that require a
!                               pointer attribute)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id
    end type

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len               :: n1
        character(n1), allocatable :: name
    end type
end module

program fpAssgn001d5
use m
    class(base(4)), pointer :: b1, b2

    allocate (child(4,20):: b1, b2)

    b1%id => null()             !<-- illegal
    nullify (b1%id)             !<-- illegal
    deallocate (b2%id)          !<-- illegal

    select type (b1)
        class is (base(4))
            b1 => b2            !<-- illegal
            nullify (b1)        !<-- illegal
            deallocate (b1)     !<-- illegal
    end select
end
