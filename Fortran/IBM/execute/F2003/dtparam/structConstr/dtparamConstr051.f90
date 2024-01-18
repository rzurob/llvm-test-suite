!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/15/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Use the expressions as the data source for the
!                               allocatable components; component is of derived
!                               type with type parameters; function result of an
!                               allocatable object.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k, dim)
        integer, kind :: k
        integer, len :: dim

        real(k) :: x(dim)
    end type

    type base (k)
        integer, kind :: k

        type(point(k,:)), allocatable :: data
    end type

    interface genPointAlloc
        module procedure genPointAlloc4
        module procedure genPointAlloc8
    end interface

    contains

    function genPointAlloc4 (r1)
        real(4), intent(in) :: r1(:)

        type(point(4,:)), allocatable :: genPointAlloc4

        allocate (genPointAlloc4, source=point(4,size(r1))(x=r1))
    end function

    function genPointAlloc8 (d1)
        double precision, intent(in) :: d1(:)

        type(point(8,size(d1))), allocatable :: genPointAlloc8

        if (all(d1 <= 0)) then
            return
        else
            genPointAlloc8 = point(8,size(d1))(d1)
        end if
    end function
end module

program dtparamConstr051
use m
    type (base(4)), allocatable :: b1
    type (base(8)), allocatable :: b2

    real(8), allocatable :: d1(:)

    logical(4), external :: precision_r4, precision_r8

    allocate (d1(0:19), source=(/(i*1.1d0, i=0,19)/))

    b1 = base(4)(genPointAlloc((/(i*1.0_4, i=1, 10)/)))

    allocate (b2, source = base(8)(genPointAlloc(d1)))

    deallocate(d1)

    !! verify b1 and b2
    if (.not. allocated(b1)) error stop 1_4

    if ((b1%data%dim /= 10) .or. (b2%data%dim /= 20)) error stop 2_4

    do i = 1, 10
        if (.not. precision_r4(b1%data%x(i), i*1.0_4)) error stop 3_4
    end do

    do i = 1, 20
        if (.not. precision_r8(b2%data%x(i), (i-1)*1.1d0)) error stop 4_4
    end do

    !! try an unallocated allocatable function result
    associate (x => base(8)(genPointAlloc((/(-i*1.0d1, i=1, 1000)/))))
        if (allocated(x%data)) error stop 5_4
    end associate

    !! try zero-size array as the allocatable return
    associate (x => base(4)(genPointAlloc((/(i*1.0, i=1,0)/))))
        if (.not. allocated(x%data)) error stop 6_4

        if ((size(x%data%x) /= 0) .or. (x%data%dim /= 0)) error stop 7_4
    end associate
end
