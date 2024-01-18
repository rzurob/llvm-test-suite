!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/25/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3.1: array component)
!                               Case: component-array-spec overrides the
!                               specification in DIMENSION attribute; use kind
!                               type parameter for allocatable/pointer
!                               components.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        integer(k), dimension(:), pointer :: i1, i2(:,:)
        real(2*k), dimension(:,:), allocatable :: r1(:), r2
    end type
end module

program dtparamArraySpec003
use m
    class(base(4)), allocatable :: b1

    type (base(8)), pointer :: b2(:)

    integer(4), target :: i4_1(10), i4_2(10, 100)
    integer(8), target :: i8_1(50), i8_2(20, 5)

    real(8) :: r8_1(10), r8_2(20, 10), r8_21(200)
    real(16), allocatable :: r16_1(:), r16_2(:,:)

    logical(4), external :: precision_r8, precision_r6

    allocate (b1, b2(10))
    allocate (r16_1(300), r16_2(20, 50))

    b1%i1 => i4_1
    b1%i2 => i4_2

    b2(2)%i1 => i8_1
    b2(1)%i2 => i8_2


    !! assign values to i4_1, i4_2, i8_1 and i8_2
    b1%i1 = (/(i, i=1, 10)/)
    b1%i2 = -1000

    b2(2)%i1 = 2_8**30 * (/(i*100, i=1, 50)/)
    b2(1)%i2 = 2_8**30 * reshape((/(i, i = 1, 100)/), (/20, 5/))


    !! assign values to r8_1, r8_2, r16_1 and r16_2
    r8_1 = (/(i*1.0d0, i=1, 10)/)
    r8_2 = reshape ((/((i*1.0d2+j, i=1,20), j=1,10)/), (/20, 10/))
    r8_21 = transfer (r8_2, r8_21)

    r16_1 = (/(i*1.0q0, i=1, 300)/)
    r16_2 = reshape ((/((i*1.0q1+j, i=1, 20), j=1,50)/), (/20, 50/))

    allocate (b1%r1(size(r8_1)), source=r8_1)
    allocate (b1%r2(size(r8_2, 2), size(r8_2, 1)))

    b1%r2 = reshape(r8_2, (/size(r8_2, 2), size(r8_2, 1)/))

    allocate (b2(6)%r1(size(r16_1)), source=r16_1)
    allocate (b2(8)%r2(10, 50), source=r16_2(::2,:))

    !! verify results
    if (any(i4_1 /= (/(i, i=1, 10)/))) error stop 1_4
    if (any(i4_2 /= -1000)) error stop 2_4

    if (any (i8_1/2_8**30/100  /= (/(i, i=1,50)/))) error stop 3_4

    k = 1

    do i = 1, 5
        do j = 1, 20
            if (i8_2(j, i)/2_8**30 /= k) error stop 4_4

            k = k + 1
        end do
    end do

    do i = 1, 10
        if (.not. precision_r8 (b1%r1(i), i*1.0d0)) error stop 5_4
    end do

    k = 1
    do i = 1, 20
        do j = 1, 10
            if (.not. precision_r8(b1%r2(j, i), r8_21(k))) error stop 6_4

            k = k + 1
        end do
    end do

    do i = 1, 300
        if (.not. precision_r6(b2(6)%r1(i), i*1.0q0)) error stop 7_4
    end do

    do j = 1, 50
        do i = 1, 10
            if (.not.  precision_r6(b2(8)%r2(i,j), (2*i-1)*1.0q1+j)) &
                    error stop 8_4
        end do
    end do
end
