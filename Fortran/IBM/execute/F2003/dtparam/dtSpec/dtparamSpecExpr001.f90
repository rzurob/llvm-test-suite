!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/21/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: variable length type parameter in
!                               derived-type-spec: deferred type parameter.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (lb, ub)
        integer, len :: lb, ub

        real :: data(lb:ub)
    end type

    contains

    subroutine setData (b, r1)
        type(base(:,:)), allocatable, intent(out) :: b
        real, allocatable, intent(in) :: r1(:)

        allocate (base(lbound(r1,1),ubound(r1,1)) :: b)

        b%data = r1
    end subroutine
end module

program dtparamSpecExpr001
use m
    type(base(:,:)), allocatable :: b1

    real, allocatable :: r1(:)

    logical(4), external :: precision_r4

    allocate (r1(-1:21), source=(/(i*1.2, i=1, 23)/))

    allocate(base(32, 42) :: b1)

    call setData (b1, r1)

    !! verify
    if ((b1%lb /= -1) .or. (b1%ub /= 21)) error stop 1_4

    do i = 1, 23
        if (.not. precision_r4 (b1%data(i-2), i*1.2)) error stop 2_4
    end do
end
