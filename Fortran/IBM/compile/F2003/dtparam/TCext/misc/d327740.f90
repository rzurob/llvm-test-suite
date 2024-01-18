! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/F2003/misc/d327740.f
! opt variations: -qnok -qnol -qreuse=none

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/20/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 327740)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type
end module

use m
    type, extends(base) :: child    ! (4,20)
        integer(k1) i
    end type

    class(child(4,20)) func1
    class(base(4,20)) func2

    external :: func1, func2
end
