! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 10/18/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the dummy-arg variable arrays used in
!                               the intrinsic assignment in the nested function
!                               calls; test intrinsic types (integer and
!                               logical(8)).
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

!! module used to test integer allocatables
module m
    contains

    integer function sumNcopy (i_alloc, i1)
        allocatable sumNcopy, i_alloc(:)
        dimension i1(:,:,:,:)

        call packInt(i_alloc, i1)
        sumNcopy = sumInt (i1, size(i1))
    end function

    subroutine packInt (i1, i2)
        allocatable i1(:)
        dimension i2(:,:,:,:)

        i1 = pack(i2, .true.)
    end subroutine

    integer function sumInt (i1, n)
        dimension i1(*)

        sumInt = sum(i1(1:n))
    end function
end module


!! module used to test the logical(8) allocatables
module m1
    contains

    integer function produceMask (mask, r1, comp)
        logical(8), allocatable :: mask(:,:)
        real(8), intent(in) :: r1(:,:), comp

        allocatable produceMask

        mask = reshape([(.true., i=1,size(r1))], shape(r1))

        produceMask = count(selectVal (mask, r1, comp))
    end function

    logical(8) function selectVal (mask, r1, minRval)
        logical(8), allocatable :: mask(:,:)
        real(8), intent(in) :: r1(:,:), minRval

        allocatable selectVal(:,:)

        mask = r1 >= minRval

        selectVal = mask
    end function
end module



program dummyArg006
    logical(8), allocatable :: mask(:,:)

    double precision d1(10, 20)

    interface
        integer function testLog8 (mask, d1, comp)
            logical(8), allocatable :: mask(:,:)
            real(8), intent(in) :: d1(:,:), comp

            allocatable testLog8
        end function
    end interface

    d1 = reshape([(i,201-i, i=1,100)], [10,20])

    call testInt

    if (testLog8 (mask, d1, 1.005d2) /= 100) stop 10

    if (any(shape(mask) /= [10, 20])) error stop 11_4

    if (any([mask] .neqv. [(.false., .true., i=1,100)])) error stop 12_4
end

subroutine testInt
use m
    integer, allocatable :: i1(:,:,:,:), i2(:), i3

    i1 = reshape([(j, j=1,450)], [3,4,5,7])

    i1 = i1 + 0

    i3 = sumNcopy (i2, i1)

    if ((.not. allocated(i1)) .or. (.not. allocated(i2)) .or. &
        (.not. allocated(i3))) error stop 1_4

    if (any(shape(i1) /= [3,4,5,7])) error stop 2_4

    if (size(i2) /= 420) error stop 3_4

    if (i3 /= 88410) error stop 4_4

    do i = 1, 420
        if (i2(i) /= i) error stop 5_4
    end do
end subroutine


integer function testLog8 (mask, d1, comp)
use m1
    logical(8), allocatable :: mask(:,:)
    real(8), intent(in) :: d1(:,:), comp

    allocatable testLog8

    testLog8 = produceMask (mask, d1, comp)
end function
