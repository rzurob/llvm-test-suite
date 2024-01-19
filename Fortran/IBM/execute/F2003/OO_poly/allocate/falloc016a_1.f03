! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/02/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (deallocation of allocated
!                               poly-allocatable array of size-zero)
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
        integer(4) :: id
    end type

    type, extends(base) :: child
    end type
end module

program falloc016a_1
use m

    class (base), allocatable :: b(:)

    allocate (child:: b(2:1))

    deallocate (b)

    if (allocated (b)) error stop 1_4

    allocate (b(1))
end
