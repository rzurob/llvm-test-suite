! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/07/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : data pointer assignment (self assignment for
!                               the poly-pointer array)
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

        contains

        procedure, pass :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name
        contains

        procedure, pass :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, 'id = ', b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, 'id = ', b%id, '; name = ', b%name
    end subroutine
end module

program fpAssgn011a1
use m
    type container
        class (base), pointer :: data (:) => null()
    end type

    type (container) :: co1

    type (child), target :: c1(10)

    class (base), pointer :: x(:)

    c1 = (/(child(i, 'c1'), i=1, 10)/)

    x => c1

    x => x(::4)

    if (size(x) /= 3) error stop 1_4

    call x(1)%print
    call x(2)%print
    call x(3)%print

    co1 = container (c1)

    co1 = container (co1%data(::3))

    if (size(co1%data) /= 4) error stop 2_4

    call co1%data(1)%print
    call co1%data(2)%print
    call co1%data(3)%print
    call co1%data(4)%print
end
