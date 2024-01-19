! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (array constructor used
!*                               for constructing poly-allocatable array
!*                               component)
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

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printChild (c)
        class (child), intent(in) :: c

        print *, c%id, c%name
    end subroutine

    subroutine printBase (c)
        class (base), intent(in) :: c

        print *, c%id
    end subroutine
end module

module m1
use m
    type container
        class (base), allocatable :: data (:)
    end type
end module

program fconstr032a
use m1
    type (container) :: co1

    class (base), allocatable :: c1, c2

    allocate (c1, source=child(1,'test1'))
    allocate (c2, source=child(2, 'test2'))

    co1 = container ((/c1, c2/))

    if (size (co1%data) /= 2) error stop 1_4

    call co1%data(1)%print
    call co1%data(2)%print
end
