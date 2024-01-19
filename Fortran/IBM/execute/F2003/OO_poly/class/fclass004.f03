! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/01/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : class keyword (intrinsic assignment, RHS can be
!*                               poly-entities)
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
        integer*4 :: id = 0
    end type

    type, extends(base) :: child
        character*15 :: name
    end type
end module

program flcass004
use m
    type (base) :: b1

    class (base), pointer :: b2

    type (child), target :: c1


    c1 = child (10, 'c1_test')

    b2 => c1

    b1 = b2

    if (b1%id /= 10) error stop 1_4
end
