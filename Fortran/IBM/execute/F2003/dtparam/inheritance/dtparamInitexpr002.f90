!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/01/2005
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: kind type parameters used in the
!                               initialization expression for the default
!                               initializations for components in the derived
!                               type definition.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, kind :: n

        character(n) :: name = repeat('a', n)
    end type

    type, extends(base) :: child (kind, seedx, seedy)
        integer, kind :: kind
        integer, kind :: seedx = -1
        integer, kind :: seedy = -2

        complex(kind) :: data(2) = cmplx(seedx, seedy, kind)
    end type

    type(child(20, 8)), save :: c1_m(2)
end module

program dtparamInitexpr002
use m
    type (child(10, 4)) c1

    logical(4) precision_x8, precision_x6

    !! verify the default initializations
    if (any(c1_m%name /= (/(repeat('a', 20), i=1,2)/))) error stop 1_4

    if (c1%name /= repeat('a', 10)) error stop 2_4

    if ((.not. precision_x6(c1_m%data(1), c1_m%data(2))) .or. &
        (.not. precision_x6(c1_m%data(1), cmplx(-1, -2, 8)))) error stop 3_4


    if ((.not. precision_x8(c1%data(1), c1%data(2))) .or. &
        (.not. precision_x8(c1%data(1), cmplx(-1, -2, 4)))) error stop 4_4
end
