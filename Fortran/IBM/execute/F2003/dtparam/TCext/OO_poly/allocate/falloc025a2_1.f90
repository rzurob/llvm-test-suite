! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc025a2_1.f
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
!*  DATE                       : 01/27/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocate (auto-deallocation for allocatable
!                               subobjects does not apply to pointers'
!                               components)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        final :: finalizeBase
    end type

    type A(k2)    ! (4)
        integer, kind               :: k2
        type(base(k2)), allocatable :: b1
    end type

    type B(k3)    ! (4)
        integer, kind        :: k3
        type(A(k3)), pointer :: a1
    end type

    contains

    subroutine finalizeBase (b)
        type (base(4)), intent(in) :: B(4)

        print *, 'finalizeBase'
    end subroutine
end module

subroutine xyz
use m
    type (B(4)) :: b1

    allocate (b1%a1)
    allocate (b1%a1%b1)
end subroutine


program falloc025a2_1
    print *, 'begin'
    call xyz

    print *, 'end'
end
