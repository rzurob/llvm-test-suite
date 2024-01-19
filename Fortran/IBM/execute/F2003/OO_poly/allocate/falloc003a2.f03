! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (array constructor used as the
!                               source-expr in ALLOCATE statement)
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


program falloc003a2
use m
    class (base), allocatable :: b1(:), b2

    type (child) :: c1 = child (10, 'c1')

    allocate (b1(2), source=(/c1, child(20, 'c2')/))

    call b1(1)%print
    call b1(2)%print

    deallocate (b1)

    allocate (b1(1), source=(/c1/))

    call b1(1)%print

    allocate (b2, source=child(30, 'b2'))

    call b2%print

    deallocate (b1)

    allocate (b1(1), source=(/b2/))

    call b1(1)%print
end
