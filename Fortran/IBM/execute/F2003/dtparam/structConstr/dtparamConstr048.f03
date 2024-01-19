! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/09/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Test the allocatable component for a derived
!                               type with type parameter; test NULL() as the
!                               data-source for components of intrinsic type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k1, k2)
        integer, kind :: k1, k2

        real(k1), allocatable :: d1
        complex(k2), allocatable :: cx(:)
    end type
end module

program dtparamConstr048
use m
    type (base(4,4)) b1
    class(base(8, 4)), allocatable :: b2(:)

    type (base(4, 8)), pointer :: b3

    b1 = base(4,4)(null(), cx=null())

    allocate (b2(10), source=base(8,4)(cx=null(), d1=null()))

    allocate (b3)

    b3 = base(4,8)(null(), null())

    !! verify b1, b2 and b3

    if (allocated(b1%d1) .or. allocated(b1%cx)) error stop 1_4

    if (allocated(b3%d1) .or. allocated(b3%cx)) error stop 2_4

    do i = 1, 10
        if (allocated(b2(i)%d1) .or. allocated(b2(i)%cx)) error stop 3_4
    end do

    call move_alloc(b1%d1, b3%d1)

    call move_alloc(b1%cx, b2(5)%cx)

    if (allocated(b3%d1) .or. allocated(b2(5)%cx)) error stop 4_4

    deallocate (b2, b3)
end
