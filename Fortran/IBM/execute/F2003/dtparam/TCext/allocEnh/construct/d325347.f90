! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/allocEnh/construct/d325347.f
! opt variations: -qck -qnok -qnol

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
!*  DATE                       : 09/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 325347)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        character(:), pointer :: name => null()
    end type
end module

use m
    character(20), target :: c = 'xlftest'

    type(base(4,20)) b1

    b1 = base(4,20) (c)

    if (b1%name%len /= 20) error stop 1_4

    if (b1%name /= 'xlftest') error stop 2_4
end
