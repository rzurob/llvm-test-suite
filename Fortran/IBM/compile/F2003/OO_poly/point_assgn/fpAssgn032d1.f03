! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/29/2005
!*
!*  DESCRIPTION                : data pointer assignment (LHS of the data
!                               assignment has to be a variable)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer id

        contains

        procedure :: makeData => produceBasePtr
    end type

    contains

    type (base) function produceBasePtr (b)
        class (base), intent(in) :: b
        pointer produceBasePtr

        allocate (produceBasePtr, source=b)
    end function
end module

program fpAssgn032d1
use m
    class (base), pointer :: b1

    allocate(b1, source=base(10))

    b1%makeData() => b1     !<-- illegal
end