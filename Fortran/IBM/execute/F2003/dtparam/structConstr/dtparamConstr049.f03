! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/10/2006
!*
!*  DESCRIPTION                : derived type parameter
!                               Test the allocatable component of a derived type
!                               with type parameters; test the allocation status
!                               using an allocatable object as the data-source;
!                               test intrinsic data type as components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k), allocatable :: data(:,:)
        character(n), allocatable :: name
    end type
end module

program dtparamConstr049
use m
    double precision, target :: d1(100)

    real(8), pointer :: p1(:,:)

    character(:), allocatable :: c

    type(base(8,:)), allocatable :: b1

    logical(4), external :: precision_r8

    p1 (1:10,0:5) => d1

    d1 = (/(i, i=0, 99)/)

    allocate (base(8,20) :: b1)

    b1 = base(8,20)(p1, c)

    if (allocated(b1%name) .or. (.not. allocated(b1%data))) error stop 1_4

    if (any(lbound(b1%data) /= (/1,0/))) error stop 2_4

    if (any(ubound(b1%data) /= (/10, 5/))) error stop 3_4

    k = 0

    do j = 0, 5
        do i = 1, 10
            if (.not. precision_r8(k*1.0d0, b1%data(i,j))) error stop 4_4

            k = k + 1
        end do
    end do



    c = 'xlftest 101'

    b1 = base(8, 11)(null(), c)

    if (allocated(b1%data) .or. (.not. allocated(b1%name))) error stop 5_4

    if (len(b1%name) /= 11) error stop 6_4

    if (b1%name /= 'xlftest 101') error stop 7_4



    p1 (1:5, 0:5) => d1(::2)

    b1 = base(8, 20) (p1, c)

    if ((.not. allocated(b1%data)) .or. (.not. allocated(b1%name))) &
        error stop 8_4

    if (any(lbound(b1%data) /= (/1,0/))) error stop 9_4
    if (any(ubound(b1%data) /= 5)) error stop 10_4

    k = 0

    do j = 0, 5
        do i = 1, 5
            if (.not. precision_r8(b1%data(i,j), k*1.0d0)) error stop 11_4

            k = k + 2
        end do
    end do
end
