! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_poly/misc/fmisc029a.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/16/2005
!*
!*  DESCRIPTION                : miscellaneous (defect 299880)
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
    type base(n1,k1)    ! (20,8)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      id

    end type

    contains

    type (base(20,8)) function f1(i)
        integer, value :: i
        dimension f1(10:10+i)

        f1%id = (/(j, j = 1, i+1)/)
    end function
end module

program fmisc029a
use m
    associate (x => f1(3))
        if (lbound(x, 1) /= 1) error stop 1_4
        if (ubound(x, 1) /= 4) error stop 2_4

        if (any (x%id /= (/1,2,3,4/))) error stop 3_4
    end associate
end