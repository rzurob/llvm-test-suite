!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/14/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the case where dummy-arg is the expr for
!                               the intrinsic assignment; use assumed-size array
!                               for intrinsic types and derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    logical, allocatable :: l1_m(:,:)

    type base
        logical(8), allocatable :: flag
    end type

    type, extends(base) :: child
        integer id
    end type

    contains

    subroutine takeASection (b1, b2, lb, ub)
        type(base), allocatable :: b1(:)
        class(base), intent(in) :: b2(0:*)
        integer, intent(in) :: lb, ub

        b1 = b2(lb:ub)
    end subroutine
end module

program dummyArg003
use m
    type(base), allocatable :: b1(:)
    logical, allocatable :: l1(:)

    l1 = (/(mod(i,6) /= 0, i=1, 100)/)

    call setL1_m (l1)

    call takeASection (b1, (/(child(l1(i), i), i=99,1,-1)/), 3, 25)

    !! verify l1_m and b1

    if ((.not. allocated(l1_m)) .or. (.not. allocated(b1))) error stop 1_4

    if (any(lbound(l1_m) /= 1) .or. any(ubound(l1_m) /= (/9, 5/))) &
            error stop 2_4

    if ((lbound(b1,1) /= 1) .or. (ubound(b1,1) /= 23)) error stop 3_4

    k = 1
    do j = 1, 5
        do i = 1, 9
            if (l1_m(i,j) .neqv. l1(27+k)) error stop 4_4

            k = k + 1
        end do
    end do

    do i = 1, 23
        if (b1(i)%flag .neqv. l1(97-i)) error stop 5_4
    end do
end

subroutine setL1_m (r)
use m, only: l1_m
    logical r(2:10, *)

    !! take a section of the dummy-arg
    l1_m = r(:,4:8)
end subroutine

