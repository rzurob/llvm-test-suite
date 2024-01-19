! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/24/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.1: array components)
!                               Case: component-array-spec overrides the
!                               specification in DIMENSION attribute; apply to
!                               type-parameters used as the bounds specifiers.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (lb, ub)
        integer, len :: lb, ub

        integer, dimension(lb:ub) :: id(0:ub-lb+1) = -1, i = -10
    end type
end module

program dtparamArraySepc001
use m
    type (base(10, 100)) :: b1
    type (base(:,:)), pointer :: b2

    b1%id = (/(i, i=0,91)/)
    b1%i = (/(i, i=b1%lb, b1%ub)/)

    if ((lbound(b1%id, 1) /= 0) .or. (ubound(b1%id, 1) /= 91)) error stop 1_4

    if ((lbound(b1%i, 1) /= 10) .or. (ubound(b1%i, 1) /= 100)) error stop 2_4

    if (any(b1%id /= (/(i, i=0,91)/)) .or. any(b1%i /= (/(i, i=10, 100)/))) &
            error stop 3_4

    allocate (base(b1%id(8), b1%i(10)) :: b2)

    if ((lbound(b2%id, 1) /= 0) .or. (ubound(b2%id, 1) /= 3)) error stop 4_4
    if ((lbound(b2%i, 1) /= 8) .or. (ubound(b2%i, 1) /= 10)) error stop 5_4

    b2%id = (/-1, 3, 1, 10/)
    b2%i = (/2, 1, 9/)

    if (any(b2%id /= (/-1, 3, 1, 10/))) error stop 6_4
    if (any(b2%i /= (/2, 1, 9/))) error stop 7_4
end
