! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/18/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : structure constructor (self data in structure
!                               constructor)
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
        integer*4 :: id = -1

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child
        character*20 :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    type container
        class (base), allocatable :: data
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

program fconstr501a
use m
    type (container) :: co1

    allocate (co1%data, source=child(1, 'data1'))

    call co1%data%print

    co1 = container (co1%data)

    call co1%data%print
end
