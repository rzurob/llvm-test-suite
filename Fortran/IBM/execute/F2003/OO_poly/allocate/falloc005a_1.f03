! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (named constants used as the
!                               source-expr)
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

    type, extends (base) :: child
        character(20) :: name

        contains

        procedure :: print => printChild
    end type

    type (child), parameter :: c_const = child (1, 'c_const')

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

program falloc005a_1
use m
    class (base), allocatable :: b1(:), b3(:)
    class (base) b2
    pointer b2

    allocate (b1(2:3), source=c_const)

    allocate (b2, source=c_const%base)

    allocate (b3 (100:101), source=c_const%base)

    call b1(2)%print
    call b1(3)%print

    call b2%print

    call b3(100)%print
    call b3(101)%print

    deallocate (b1, b2, b3)
end