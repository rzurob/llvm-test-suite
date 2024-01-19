! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (this testcase actually
!*                               tests the data-pointer-assignment)
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

        print *, b%id
    end subroutine

    subroutine printchild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

use m

    type container
        class (base), pointer :: data => null()
    end type

    type (container) :: co(2)

    type (base), target :: b1
    type (child), target :: c1 = child (2, 'child_type_c1')

    b1 = base (1)

    co(1)%data => b1
    co(2)%data => c1

    call co(1)%data%print
    call co(2)%data%print
end
