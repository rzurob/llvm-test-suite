! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/24/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Component of a parent component is
!                               provided by component itself and the parent
!                               component.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type, extends(base) :: child (m)
        integer, len :: m

        integer(k) :: id(m)
    end type
end module

program dtparamConstr011d
use m
    type (child(8, 11, 22)) c1

    c1 = child(8, 11, 22)((/(i*1.0d0, i=1, 11)/), base=base(8,11)(1.2d0), &
            id = (/(j, j=1, 22)/))
end
