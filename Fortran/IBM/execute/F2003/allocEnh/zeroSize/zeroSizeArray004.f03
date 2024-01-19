! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/29/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Use function result as the zero-sized arrays for
!                               assigning to an allocatable array; functions are
!                               of allocatable and pointer type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8) :: data

        contains

        procedure :: getData
        procedure :: genBasePtr
        procedure :: genBaseAlloc
    end type

    contains

    elemental real(8) function getData (b)
        class(base), intent(in) :: b

        getData = b%data
    end function

    type(base) function genBasePtr (b, lb, ub)
        class(base), intent(in) :: b
        integer, intent(in) :: lb, ub

        pointer genBasePtr(:)

        allocate (genBasePtr(lb:ub), source=b)
    end function

    function genBaseAlloc (b, lb, ub)
        class(base), intent(in) :: b
        integer, intent(in) :: lb, ub

        class(base), allocatable :: genBaseAlloc(:)

        allocate(genBaseAlloc(ub-lb+1), source=b)
    end function
end module

program zeroSizeArray004
use m
    type(base), allocatable :: b1(:), b3(:)

    class(base), pointer :: b2

    allocate (b2, source=base(dlog(atan(1.2d0))))

    b1 = b2%genBasePtr(10, 1)

    if (.not. allocated(b1)) error stop 1_4

    if (size(b1) /= 0) error stop 2_4

    b3 = b2%genBaseAlloc (0, -1)

    if (.not. allocated(b3)) error stop 3_4

    if (size(b3) /= 0) error stop 4_4

    if (size(b3%getData()) /= 0) error stop 5_4
    if (size(b1%getData()) /= 0) error stop 6_4
end
