! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (use of keyword for
!*                               argument; basic test)
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
    end type

    type, extends (base) :: child
        character*20 :: name = 'no-name'
    end type

    contains

    logical function b1LEb2 (b1, b2)
        class (base), intent(in) :: b1, b2

        b1LEb2 = (b1%id <= b2%id)
    end function
end module


program fArg001a
use m
    type (base) :: b1
    type (child) :: c1 = child (2, 'c1')

    if (b1LEb2(b2 = child(1, 'test'), b1 = base(3))) error stop 1_4

    b1%id = 3

    if (.not. b1LEb2 (b2 = b1, b1 = c1)) error stop 2_4
end
