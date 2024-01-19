! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/27/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 340786)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m1
    type base(k1,n1)
        integer, kind :: k1
        integer, len :: n1
    end type
end module

module m2
use m1, oldBase => base
    type base(k2,n2)
        integer, kind :: k2
        integer, len :: n2
        type(oldBase(k2,n2)) :: ob
    end type
end module

use m2
! ICEd with any of these:
type(base(4,20)) b0
type(oldBase(4,20)) b2
type(oldBase(4,:)), allocatable :: b3
end
