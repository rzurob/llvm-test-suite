! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (pointer assignment causes
!*                               the association status to become disassociated;
!*                               poly-pointer as LHS)
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
    end type

    type, extends(base) :: child
    end type
end module

program fpAssgn010a
use m
    type (base), target :: b1

    class(base), pointer :: b_ptr1, b_ptr2
    type (child), pointer :: c_ptr
    class (child), pointer :: c_ptr2

    b_ptr1 => null()
    b_ptr2 => b_ptr1

    if (associated(b_ptr1) .or. associated(b_ptr2)) error stop 1_4

    allocate (b_ptr2, c_ptr)

    deallocate (b_ptr2, c_ptr)    ! b_ptr2 and c_ptr are disassociated

    if (associated(b_ptr2) .or. associated (c_ptr)) error stop 2_4

    b_ptr1 => b1
    b_ptr1 => c_ptr

    if (associated(b_ptr1)) error stop 3_4

    b_ptr1 => b1
    b_ptr1 => b_ptr2

    if (associated(b_ptr1)) error stop 4_4

    c_ptr2 => c_ptr

    b_ptr1 => c_ptr2

    if (associated(c_ptr2) .or. associated(b_ptr1)) error stop 5_4
end
