! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/19/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (TARGET in associated()
!                               where the storage unit/size are compared to the
!                               pointer)
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
        integer(8) :: id
    end type

    type, extends(base) :: child1
    end type

    type, extends(base) :: child2
        character(20) :: name
    end type
end module

program fpAssgn500a
use m
    class (base), pointer :: b1, b2, b3(:), b4(:)

    type (child1), target :: c11, c12(5)
    type (child2), target :: c21, c22(3:6)

    b1 => c11%base
    b2 => c21%base

    b3 => c12%base
    b4 => c22%base

    if (.not. associated (b1, c11)) error stop 1_4

    if (associated (b2, c21) .or. (.not. associated (b2, c21%base))) error stop 2_4

    if (.not. associated (b3, c12)) error stop 3_4

    if (associated (b4, c22) .or. (.not. associated(b4,c22%base))) error stop 4_4
end