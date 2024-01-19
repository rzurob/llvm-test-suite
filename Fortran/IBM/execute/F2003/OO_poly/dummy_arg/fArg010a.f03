! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/10/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (VALUE attribute; pointer
!*                               component redefined in pointer assignment; not
!*                               affect the actual arg)
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
    end type

    type, extends(base) :: child
        character*20 :: name
    end type

    type (base), target :: b1 = base (10)
    type (child), target :: c1 = child (20, 'c1')

    type (child), target :: c2 (2:4)
    type (base), target :: b2 (3:7)
end module

module m1
use m
    type container
        class (base), pointer :: data => null()
    end type

    type container1
        class (base), pointer :: data(:) => null()
    end type
end module

program fArg010a
use m1
    type (container) :: co
    type (container1) :: co1

    call associateScalar (co, c1)

    if (associated (co%data)) error stop 1_4

    call associateArray1 (co1, b2)

    if (associated (co1%data)) error stop 2_4

    contains

    subroutine associateScalar (c, b)
        type (container), value :: c
        class (base), target, intent(in) :: b

        c%data => b
    end subroutine

    subroutine associateArray1 (c, b)
        type (container1), value :: c
        class (base), target, intent(in) :: b(:)

        c%data => b
    end subroutine
end
