! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/02/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Structure constructor with component-spec
!                               is not available if it has inaccessible
!                               components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        private

        real(k) :: data(n)
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        private

        integer(k) :: id
        character(l) :: name
    end type

    type(base(4, 10)) :: b1
end module

program dtparamConstr018d
use m
    type (child(8, 433, 20)) :: c1

    !! the use of structure constructor is illegal
    b1 = base(4, 10)(1.0)
    c1 = child(8, 433, 20)((/(i*1.0d0, i=1, 433)/), 20, 'xlftest')
end
