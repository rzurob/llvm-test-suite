! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (passed-object dummy-arg)
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

        procedure, pass (b1) :: lessThan => b1LessThanB2
        procedure, pass (b2) :: greaterThan => b1LessThanB2
    end type

    contains

    logical function b1LessThanB2 (b1, b2)
        class (base), intent(in) :: b1, b2

        b1LessThanB2 = (b1%id < b2%id)
    end function
end module


program fArg008
use m
    type (base) :: b1
    class (base), allocatable :: b2

    allocate (b2, source=base(2))

    b1%id = 1

    if (.not. b1%lessThan (b2)) error stop 1_4

    if (.not. b2%greaterThan (b1)) error stop 2_4
end
