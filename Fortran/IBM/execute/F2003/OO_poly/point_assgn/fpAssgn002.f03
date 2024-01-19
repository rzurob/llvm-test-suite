! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (C717: if data-target is
!*                               unlimited poly, data pointer shall be unlimited
!*                               poly, or sequence type, or a type with BIND
!*                               attribute; part1 unlimited poly)
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
        integer*4 :: id
    end type
end module

program fpAssgn002
use m

    class (*), pointer :: x1, x2

    integer*4, target :: y = 10
    type (base), target :: b1

    type(base), allocatable, target :: b_alloc
    class (base), pointer :: b_ptr

    x2 => y
    x1 => x2

    if (.not. associated (x1, y)) error stop 1_4

    x2 => b1
    x1 => x2

    if (.not. associated (x1, b1)) error stop 2_4

    nullify (x2)
    x1 => x2

    if (associated(x1) .or. associated(x2)) error stop 3_4

    allocate (b_alloc, b_ptr)

    x2 => b_alloc
    x1 => x2

    if (.not. associated (x1, b_alloc)) error stop 4_4

    x2 => b_ptr
    x1 => x2

    if (.not. associated (x1, b_ptr)) error stop 5_4

    deallocate (b_ptr)
end
