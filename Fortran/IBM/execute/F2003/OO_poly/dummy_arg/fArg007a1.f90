! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (disassociated pointer and
!*                               unallocated allocatables use in intrinsic
!*                               function extends_type_of)
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

    type, extends (base) :: child
        character*20 :: name
    end type

    class (*), allocatable :: x1 (:)

    class (base), pointer :: b_ptr => null()
end module

program fArg007a1
use m
    class (*), pointer :: x2 (:)
    class (base), allocatable :: b1

    nullify (x2)

    if (.not. extends_type_of (b1, x2)) error stop 1_4

    if (.not. extends_type_of (b_ptr, x1)) error stop 2_4

    if (extends_type_of (x1, b1)) error stop 3_4

    if (extends_type_of (x2, b_ptr)) error stop 4_4
end
