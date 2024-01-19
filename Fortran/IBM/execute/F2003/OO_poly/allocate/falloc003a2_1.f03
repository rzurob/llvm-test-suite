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
        integer(4) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(20) :: name

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
    class (base), allocatable :: b1(:), b2

    type (child) :: c1 = child (10, 'c1')

    allocate (b1(2), source=(/base (10), base(20)/))

    call b1(1)%print
    call b1(2)%print

    allocate (b2)

    b2%id = 30

    deallocate (b1)

    allocate (b1(2), source = (/c1%base, b2/))

    call b1(1)%print
    call b1(2)%print
end
