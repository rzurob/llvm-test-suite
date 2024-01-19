! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 03/03/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: C490: Data target for the proc-pointer
!                               component.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        procedure(real(k)), nopass, pointer :: p1
        real(k) :: data(n)
    end type

    type (base(8,:)), allocatable :: b1
end module

program dtparamConstr021d
use m
    type (base(4, 21)) b2

    real(4), pointer :: r1
    real(8), pointer :: d1

    allocate (base(8,45) :: b1)

    b1 = base(8,45)(data=(/(i*1.0d0, i=1,45)/), p1=d1)

    b2 = base(4,21)(data=1.0, p1=r1)
end
