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
    type base
        integer(4) id
    end type

    type, extends(base) :: child
        character (20) :: name
    end type
end module

program fintrMisc001
use m
    type (child) c1
    class (base), allocatable :: b1

    if (same_type_as (b1, B=c1)) error stop 1_4
    if (extends_type_of (mold=c1, a=b1)) error stop 2_4
end
