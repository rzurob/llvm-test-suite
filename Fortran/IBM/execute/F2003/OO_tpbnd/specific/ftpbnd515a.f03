! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : specific type-bound (one function bound to two
!*                               bindings; elemental function; check if bounds
!                                have any effect on the behavior)
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

    type, extends(base) :: child1
        character*20 :: name

        contains

        procedure, pass (c1) :: compare => compareC1C2
    end type

    type, extends(base) :: child2
        character*15 :: name

        contains

        procedure, pass (c2) :: compare => compareC1C2
    end type

    contains

    elemental integer*4 function compareC1C2 (c1, c2)
        class (child1), intent(in) :: c1
        class (child2), intent(in) :: c2

        compareC1C2 = (c1%id - c2%id)
    end function
end module

program ftpbnd515a
use m
    type (child1) :: c1(10)
    type (child2) :: c2(2:11)

    c1 = (/(child1 (i, 'test'),i=1,10)/)

    c2 = (/(child2 (i, 'test'),i=2,11)/)


    if (any (c1%compare (c2) /= -1)) error stop 1_4

    if (any (c2%compare (c1) /= -1)) error stop 2_4
end