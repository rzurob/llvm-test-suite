! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn010a.f
! opt variations: -qnok -qnol -qnodeferredlp

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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(base) :: child(k2,n2)    ! (4,20,4,20)
        integer, kind :: k2
        integer, len  :: n2
    end type
end module

program fpAssgn010a
use m
    type (base(4,20)), target :: b1

    class(base(4,:)), pointer :: b_ptr1
    class(base(4,20)), pointer :: b_ptr2
    type (child(4,20,4,20)), pointer :: c_ptr
    class (child(4,:,4,:)), pointer :: c_ptr2

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
