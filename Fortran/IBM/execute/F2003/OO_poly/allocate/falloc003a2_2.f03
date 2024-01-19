! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (array constructor as the source-expr)
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
        integer(4) :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module


program falloc003a2_1
use m
    type (base), allocatable :: b1(:)
    type (child), pointer :: c_ptr(:)

    type (child) :: c1 = child (10, 'c1')
    type (child) :: c2

    allocate (b1(2), source=(/c1%base, base(20)/))

    call b1(1)%print
    call b1(2)%print

    allocate (c_ptr (2), source= (/c2, c1/))

    call c_ptr(1)%print
    call c_ptr(2)%print

    deallocate (c_ptr, b1)
end
