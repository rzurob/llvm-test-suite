! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/15/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (nonpoly allocatable
!                               array component with poly-array data-source; use
!                               rank-one arrays; use associate construct)
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
        type (base), allocatable :: data(:)
    end type
end module

program fconstr050a
use m1
    class (base), allocatable :: b1(:)

    allocate (b1(-1:0), source = (/child(1,'test1'), child(2,'test2')/))

    associate (x => container (b1))
        if (.not. allocated (x%data)) error stop 1_4

        if ((lbound(x%data,1) /= -1) .or. (ubound(x%data,1) /= 0)) error stop 2_4
        print *, x%data

        call x%data(-1)%print
        call x%data(0)%print
    end associate
end