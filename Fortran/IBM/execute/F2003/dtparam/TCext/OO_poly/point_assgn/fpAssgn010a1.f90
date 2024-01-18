! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn010a1.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (assignment causes pointers
!*                               to be disassociated; non-poly pointer as LHS)
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
    end type

    type, extends(base) :: child    ! (4)
        integer(k1) id
    end type
end module

program fpAssgn010a1
use m
    type (base(4)), pointer :: b_ptr1
    class (base(4)), pointer :: b_ptr2
    type (child(4)) :: c_ptr1, c1
    pointer c_ptr1
    target c1
    class (child(4)), pointer :: c_ptr2

    b_ptr1 => c1%base

    if (.not. associated (b_ptr1)) error stop 1_4

    c_ptr1 => c1
    b_ptr2 => c1

    allocate (c_ptr2)

    deallocate (c_ptr2)

    c_ptr1 => c_ptr2        ! c_ptr1 diassociated

    if (associated (c_ptr1) .or. associated(c_ptr2)) error stop 2_4

    c_ptr2 => c1

    nullify (c_ptr2)
    b_ptr2 => c_ptr2        ! b_ptr2 dissociated

    if (associated (b_ptr2) .or. associated (c_ptr2)) error stop 3_4

    b_ptr1 => b_ptr2        ! b_ptr1 dissociated

    if (associated (b_ptr1)) error stop 4_4
end
