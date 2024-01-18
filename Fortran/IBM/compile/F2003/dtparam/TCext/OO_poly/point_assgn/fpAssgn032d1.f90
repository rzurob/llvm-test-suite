! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn032d1.f
! opt variations: -ql

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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure :: makeData => produceBasePtr
    end type

    contains

    type (base(4)) function produceBasePtr (b)
        class (base(4)), intent(in) :: b
        pointer produceBasePtr

        allocate (produceBasePtr, source=b)
    end function
end module

program fpAssgn032d1
use m
    class (base(4)), pointer :: b1

    allocate(b1, source=base(4)(10))

    b1%makeData() => b1     !<-- illegal
end
