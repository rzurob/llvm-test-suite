! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/22/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: C485: component-spec in structure
!                               constructor.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type, extends(base) :: child
        complex(k), allocatable :: cx
    end type
end module

program dtparamConstr002
use m
    type (child(4, 30)) c1

    logical(4), external :: precision_r4

    c1 = child(k=4, n=30)(data=(/(i*1.2e-1, i=1,30)/), cx=null())

    if (allocated(c1%cx)) error stop 1_4

    do i = 1, c1%n
        if (.not. precision_r4(c1%data(i), i*1.2e-1)) error stop 2_4
    end do
end
