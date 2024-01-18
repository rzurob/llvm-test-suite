! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/08/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the case where the selector is a function
!                               result of rank two array of derived type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), allocatable :: data
    end type

    contains

    function genBase (b, b2)
        class(base), intent(in) :: b(:,:), b2(:,:)

        type(base) genBase(size(b,1),size(b,2))

        if (any(shape(b) /= shape(b2))) error stop 10

        do i = 1, size(b,1)
            do j = 1, size(b,2)
                if ((.not. allocated(b(i,j)%data)) .or. &
                    (.not. allocated(b2(i,j)%data))) cycle

                if (same_type_as(b(i,j)%data, b2(i,j)%data)) &
                    genBase(i,j) = base(b2(i,j)%data)
            end do
        end do
    end function
end module

program associate005a
use m
    class(base), allocatable :: b1(:,:)
    type(base), allocatable :: b2, b3(:,:)

    b2 = base(1)

    allocate (b1(3,3), source=reshape((/(base(i*1.2), i=1,3), &
        (base(i), i=1,3), (base(i*1_8), i=1,3)/), (/3,3/)))

    associate (x => genBase(reshape((/(b2, i=1,9)/),(/3,3/)), b1), &
        y => genBase(b1,reshape((/(b2, i=1,9)/),(/3,3/))))
        b3 = x

        if ((.not. allocated(b3)) .or. any(shape(b3) /= (/3,3/))) error stop 1_4
        do i = 1, 3
            if (allocated(b3(i,1)%data) .or. allocated(b3(i,3)%data)) &
                error stop 2_4

            if (.not. allocated(b3(i,2)%data)) error stop 3_4

            select type (u => b3(i,2)%data)
                type is (integer)
                    u = u + 10

                class default
                    error stop 5_4
            end select
        end do

        !! the following line will NOT change values of b3
        b3 = genBase(y,b3)

        if ((.not. allocated(b3)) .or. any(shape(b3) /= (/3,3/))) error stop 6_4
        do i = 1, 3
            if (allocated(b3(i,1)%data) .or. allocated(b3(i,3)%data)) &
                error stop 7_4

            if (.not. allocated(b3(i,2)%data)) error stop 8_4

            select type (u => b3(i,2)%data)
                type is (integer)
                    if (u /= 10+i) error stop 9_4

                class default
                    error stop 10_4
            end select
        end do
    end associate
end
