!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/28/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 337288)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real data(n)
        integer i(n)
    end type
end module

use m
    type(base(:)), allocatable :: b1

    integer, allocatable :: i1(:)
    real, allocatable :: r1(:)

    logical(4), external :: precision_r4

    allocate (base(10) :: b1)

    b1%i = [(i, i=1,10)]
    b1%data = sqrt(1.0*b1%i)

    r1 = b1%data
    i1 = b1%i

    if ((.not. allocated(r1)) .or. (.not. allocated(i1))) error stop 1_4

    if ((size(r1) /= 10) .or. (size(i1) /= 10)) error stop 2_4

    do i = 1, 10
        if (i1(i) /= i) error stop 3_4

        if (.not. precision_r4(r1(i), sqrt(i*1.0))) error stop 4_4
    end do
end
