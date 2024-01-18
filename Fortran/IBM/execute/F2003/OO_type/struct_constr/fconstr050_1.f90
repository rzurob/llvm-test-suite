! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (non-poly allocatable
!                               component with poly-entites as the data-source;
!                               use scalars and array elements)
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
        integer(4) id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15) name

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
        type (base), allocatable :: data
    end type
end module

program fconstr050_1
use m1
    type (container) :: co1, co2
    class (base), allocatable :: b1(:), b2(:,:)

    allocate (b1(-1:0), source=(/child(1, 'test1'), child(2, 'test2')/))

    allocate (b2(0:1, -1:0), source=child(10, 'test10'))

    !! in an associate construct
    associate ( x => container (b1(0)), y => container (b2(0,0)))
        call x%data%print
        call y%data%print
    end associate

    !! involve intrinsic assignments
    co1 = container (b1(-1))

    co2 = container (b2(1,0))

    call co1%data%print

    call co2%data%print
end

