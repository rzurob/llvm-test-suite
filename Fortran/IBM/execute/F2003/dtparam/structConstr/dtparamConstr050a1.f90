!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/14/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Use of the unallocated allocatable in the source
!                               expression; test that the component allocation
!                               status is unallocated.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (dim)
        integer, len :: dim

        real :: x (dim)
    end type

    type base (n)
        integer, len :: n

        type(point(n)), allocatable :: data(:)
    end type
end module

program dtparamConstr050a1
use m
    type(point(:)), allocatable :: p1(:)

    type(base(10)), allocatable :: b1

    class(base(:)), pointer :: b2(:)

    !! test 1: use the intrinsic assignment
    b1 = base(10)(p1)

    if (.not. allocated(b1)) error stop 1_4

    if (allocated(b1%data)) error stop 2_4

    !! test 2: use in the source-expr
    allocate(b2(0:9), source=base(20)(p1))

    if (.not. associated(b2)) error stop 3_4

    if ((lbound(b2, 1) /= 0) .or. (ubound(b2,1) /= 9)) error stop 4_4

    if (b2%n /= 20) error stop 5_4

    do i = 0, 9
        if (allocated(b2(i)%data)) error stop 6_4
    end do

    !! test 3: in an associate construct
    associate (x => base(100)(data=p1))
        if (allocated(x%data)) error stop 7_4

        if (x%n /= 100) error stop 8_4
    end associate
end
