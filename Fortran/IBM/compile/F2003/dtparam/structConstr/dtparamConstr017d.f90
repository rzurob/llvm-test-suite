!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/01/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Structure constructor not available for
!                               the derived type that is not accessible; refer
!                               to the structure constructor of such types.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, private :: base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type, private, extends(base) :: child(l)
        integer, len :: l

        character(l) :: name
    end type

    type (base(8, 12)) :: b1
    type (child(4, 25, 20)) :: c1
end module

program dtparamConstr017d
use m
    !! structure constructor is not available for the inaccessible types
    b1 = base(8,12)((/(i*1.12d0, i=1, 12)/))
    c1 = child(4, 25, 20)(1.2, 'c1 in the main')
end
