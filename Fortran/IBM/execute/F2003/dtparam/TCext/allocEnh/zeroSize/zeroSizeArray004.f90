! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/zeroSize/zeroSizeArray004.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
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
    type base(n1,k1)    ! (20,8)
        integer, kind :: k1
        integer, len  :: n1
        real(k1)      :: data

        contains

        procedure :: getData
        procedure :: genBasePtr
        procedure :: genBaseAlloc
    end type

    contains

    elemental real(8) function getData (b)
        class(base(*,8)), intent(in) :: b

        getData = b%data
    end function

    type(base(:,8)) function genBasePtr (b, lb, ub)
        class(base(*,8)), intent(in) :: b
        integer, intent(in) :: lb, ub

        pointer genBasePtr(:)

        allocate (genBasePtr(lb:ub), source=b)
    end function

    function genBaseAlloc (b, lb, ub)
        class(base(*,8)), intent(in) :: b
        integer, intent(in) :: lb, ub

        class(base(:,8)), allocatable :: genBaseAlloc(:)

        allocate(genBaseAlloc(ub-lb+1), source=b)
    end function
end module

program zeroSizeArray004
use m
    type(base(:,8)), allocatable :: b1(:), b3(:)

    class(base(:,8)), pointer :: b2

    allocate (b2, source=base(20,8)(dlog(atan(1.2d0))))

    b1 = b2%genBasePtr(10, 1)

    if (.not. allocated(b1)) error stop 1_4

    if (size(b1) /= 0) error stop 2_4

    b3 = b2%genBaseAlloc (0, -1)

    if (.not. allocated(b3)) error stop 3_4

    if (size(b3) /= 0) error stop 4_4

    if (size(b3%getData()) /= 0) error stop 5_4
    if (size(b1%getData()) /= 0) error stop 6_4
end
