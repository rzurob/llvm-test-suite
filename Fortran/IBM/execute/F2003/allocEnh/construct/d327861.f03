! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/8/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 327861)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, allocatable :: ids(:)

        contains

        final :: finalizeBaseRank1
    end type

    contains

    subroutine finalizeBaseRank1 (b1)
        type(base), intent(inout) :: b1(:)

        print *, 'finalizeBaseRank1'

        print *, lbound(b1,1), ubound(b1,1)

        do i = lbound(b1,1), ubound(b1,1)
            if (allocated(b1(i)%ids)) deallocate(b1(i)%ids)
        end do
    end subroutine
end module

use m
    type(base), pointer :: b1(:)
    type(base) b2(10)

    allocate(b1(10))

    do i = 1, 10
        b1(i)%ids = [integer :: ]

        b2(i)%ids = [(j, j=1, i)]
    end do

    b1 = b2

    if (.not. associated(b1)) error stop 10

    do i = 1, 10
        if ((.not. allocated(b1(i)%ids) .or. (size(b1(i)%ids) /= i))) error stop 1

        do j = 1, i
            if (b1(i)%ids(j) /= j) error stop 3
        end do

        if (any(b2(i)%ids /= [(j, j=1,i)])) error stop 2
    end do
end