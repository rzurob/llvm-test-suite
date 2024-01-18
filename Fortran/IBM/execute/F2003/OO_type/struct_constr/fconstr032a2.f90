! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/26/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (use an array for the
!                               allocatable component in the structure
!                               constructor; test the bounds and dynamic types
!                               of the component)
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
        integer*4 :: id = 1

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character*20 :: name = 'no-name'

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
        class (base), allocatable :: data(:)
    end type
end module

program fconstr032a2
use m1
    type (container) :: co1

    type (child) :: c1 (2:4)
    type (base) :: b1 (2:4)

    b1 (2) = base (2)
    b1 (3) = base (3)
    b1 (4) = base (4)

    c1 (2) = child (2, 'test2')
    c1 (3) = child (3, 'test3')
    c1 (4) = child (4, 'test4')


    !! use b1 as the expression
    co1 = container (b1)

    if (size (co1%data) /= 3) error stop 1_4

    if ((lbound (co1%data, 1) /= 2) .or. (ubound (co1%data, 1) /= 4)) error stop 2_4

    do i = 2, 4
        call co1%data(i)%print
    end do


    !! use c1%base as the expression
    co1 = container (c1%base)

    if (size (co1%data) /= 3) error stop 3_4

    if ((lbound (co1%data, 1) /= 1) .or. (ubound (co1%data, 1) /= 3)) error stop 4_4

    do i = 1, 3
        call co1%data(i)%print
    end do

end
