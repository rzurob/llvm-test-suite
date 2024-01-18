! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (deallocate statement
!*                               for non-poly pointer assigned to poly-pointer,
!*                               the dynamic types are different and deallocate
!*                               will fail)
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

    type, extends(base) :: child
        character*16 :: name
    end type
end module

program fpAssgn024d
use m
    class (base), pointer :: b_ptr
    type (base), pointer :: b1

    type (child), pointer :: c1

    integer*4 errno

    errno = 0
    allocate (c1)

    b_ptr => c1

    b1 => c1%base

    deallocate (b1, stat=errno)

    if (errno /= 2) error stop 1_4

    b1 => b_ptr

    deallocate (b1, stat=errno)

    if (errno /= 2) error stop 2_4
end
