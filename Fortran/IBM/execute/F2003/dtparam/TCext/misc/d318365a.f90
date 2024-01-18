! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/F2003/misc/d318365a.f
! opt variations: -qck -qnok -qnol -qnodeferredlp

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
!*  DATE                       : 04/18/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 318365)
!                               Case 2: ICE
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        character(:), allocatable :: c

        contains

        procedure :: concatBase
        generic :: operator (//) => concatBase
    end type

    contains

    type (base(4,20)) function concatBase (b1, b2)
        class(base(4,*)), intent(in) :: b1, b2

        if (allocated(b1%c) .and. allocated(b2%c)) then
            allocate (concatBase%c, source=b1%c//b2%c)
        end if
    end function
end module

use m
    type(base(4,:)), pointer :: b1(:)
    character(8), allocatable :: c1

    allocate (c1, source='xlftest ')

    allocate (b1(10), source=(/(base(4,20)(c1//char(ichar('0')+i)), i=0,9)/))

    b1(2) = b1(1)//b1(3)

    if (b1(2)%c /= 'xlftest 0xlftest 2') error stop 1_4
end
