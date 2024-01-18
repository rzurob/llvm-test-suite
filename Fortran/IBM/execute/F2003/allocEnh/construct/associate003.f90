! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/07/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the selector being a pointer with
!                               remapped bounds and use the associate-name as
!                               expr in intrinsic assignment.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), pointer :: data => null()
        character(:), allocatable :: names(:)
    end type
end module

program associate003
use m
    class(base), target, allocatable :: b1(:)

    class(base), pointer :: b2(:,:)

    type(base), allocatable :: b3(:,:), b4(:)

    logical(4), external :: precision_r8

    allocate (b1(30))

    do i = 1, 30
        allocate (b1(i)%data, source=i*1.0d0)
        b1(i)%names = (/repeat(achar(i+64),i)/)
    end do

    b2(0:3, 0:2) => b1(::2)

    associate (x => b2, y => (/b2/))
        b3 = x

        b4 = y
    end associate

    if ((.not. allocated(b3)) .or. (.not. allocated(b4))) error stop 1_4

    if (any(lbound(b3) /= (/0,0/)) .or. any(ubound(b3) /= (/3,2/))) &
        error stop 2_4

    if ((lbound(b4,1) /= 1) .or. (ubound(b4,1) /= 12)) error stop 3_4

    k = 1

    do j = 0, 2
        do i = 0, 3
            if (.not. associated(b3(i,j)%data, b1(k)%data)) error stop 4_4

            if (.not. associated(b4((k+1)/2)%data, b1(k)%data)) error stop 5_4

            if (.not. associated(b4((k+1)/2)%data, b3(i,j)%data)) error stop 6_4
            if (.not. precision_r8(b3(i,j)%data, k*1.0d0)) error stop 7_4

            if ((len(b3(i,j)%names) /= k) .or. (size(b3(i,j)%names) /= 1)) &
                error stop 8_4

            if ((len(b4((k+1)/2)%names) /= k) .or. &
                (size(b4((k+1)/2)%names) /= 1)) error stop 9_4

            if (b3(i,j)%names(1) /= b4((k+1)/2)%names(1)) error stop 10_4

            if (b3(i,j)%names(1) /= repeat(achar(k+64),k)) error stop 11_4

            k = k + 2
        end do
    end do

end
