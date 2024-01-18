!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/22/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: C487 for component-spec: each keyword MUST
!                               specify a component name of the derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k = 8
        integer, len :: n = 20

        real(k) :: data(n)
    end type
end module

program dtparamConstr001d
use m
    type (base(8, 20)) :: b1
    class(base(8,:)), allocatable :: b2

    !! illegal syntax for structure constructor
    b1 = base(k=8, n=20, data=(/(i*1.0d0, i=1, 20)/))

    !! illegal syntax for structure constructor
    allocate (b2, source=base(k=8, n=25, data=(/(i*1.0d0, i=1, 25)/)))
end
