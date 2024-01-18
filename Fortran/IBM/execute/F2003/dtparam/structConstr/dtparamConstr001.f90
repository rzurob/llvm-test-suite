!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/22/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Keyword usage for type parameters and
!                               components in a structure constructor.
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

    class(base(8,:)), pointer :: b1
end module

program dtparamConstr001
use m
    type (child(4, 30)) c1

    logical(4), external :: precision_r4, precision_r8, precision_x8

    allocate (b1, source = base(k=8, n=25)(data=(/(i*1.0d0, i=1, 25)/)))

    c1 = child(k=4, n=30)(data=(/(i*1.2e-1, i=1,30)/), cx=cmplx(1.0, 2.0))

    !! verify the results
    do i =1, b1%n
        if (.not. precision_r8(b1%data(i), i*1.0d0)) error stop 1_4
    end do

    do i = 1, c1%n
        if (.not. precision_r4(c1%data(i), i*1.2e-1)) error stop 2_4
    end do

    if (.not. allocated(c1%cx)) error stop 3_4

    if (.not. precision_x8(c1%cx, (1.0, 2.0))) error stop 4_4
end
