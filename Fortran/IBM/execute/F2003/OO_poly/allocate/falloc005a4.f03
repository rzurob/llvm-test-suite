! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : ALLOCATE (variables with poly-allocatable
!                               components in source-expr)
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

module m1
use m
    type container
        class (base), allocatable :: data (:)
    end type
end module

program falloc005a4
use m1
    class (container), allocatable :: b1
    type (container), pointer :: b2

    allocate (b2)
    allocate (b2%data (0:1), source=(/child(1,'temp1'),child(2,'temp2')/))

    allocate (b1, source = b2)

    if ((lbound (b1%data,1) /= 0) .or. (ubound(b2%data,1) /= 1)) error stop 1_4

    call b1%data(0)%print
    call b1%data(1)%print
end
