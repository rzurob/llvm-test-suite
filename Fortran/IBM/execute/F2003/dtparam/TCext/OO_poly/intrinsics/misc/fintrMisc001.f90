! GB DTP extension using:
! ftcx_dtp -qnock -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/intrinsics/misc/fintrMisc001.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : miscellaneous items in intrinsics (defect
!                               294632)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
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
    end type

    type, extends(base) :: child(n1)    ! (4,20)
        integer, len  :: n1
        character(n1) :: name
    end type
end module

program fintrMisc001
use m
    type (child(4,20)) c1
    class (base(4)), allocatable :: b1

    if (same_type_as (b1, B=c1)) error stop 1_4
    if (extends_type_of (mold=c1, a=b1)) error stop 2_4
end
