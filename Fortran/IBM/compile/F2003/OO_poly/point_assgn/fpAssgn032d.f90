! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/29/2005
!*
!*  DESCRIPTION                : data pointer assignment (LHS of a pointer
!                               assignment must ne a variable)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    class(*) function returnNull ()
        pointer returnNull

        returnNull => null()
    end function
end module

program fpAssgn032d
use m
    class(*), pointer :: x1(:) => null()

    returnNull() => null()      !<-- illegal

    returnNull() => x1          !<-- illegal
end
