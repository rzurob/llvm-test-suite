! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/20/2006
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the intrinsic assignment in a case
!                               construct.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real(8), allocatable :: data(:,:)

        contains

        procedure :: size => dataSize
    end type

    contains

    integer function dataSize (b)
        class(base), intent(in) :: b

        allocatable dataSize

        if (allocated(b%data)) then
            dataSize = size(b%data)
        else
            dataSize = -1
        end if
    end function

    subroutine allocDouble (d1, b)
        real(8), allocatable :: d1(:)
        class(base), intent(in) :: b

        select case (b%size())
            case (-1)
                if (allocated(d1)) deallocate(d1)

            case (0:)
                d1 = pack(b%data, .true.)

            case default
                stop 10
        end select
    end subroutine
end module

program case001
use m
    type(base), pointer :: b1
    double precision, allocatable :: d1(:)

    logical(4), external :: precision_r8

    allocate(b1, d1(0:100))

    call allocDouble (d1, b1)

    if (allocated(d1)) error stop 1_4

    allocate (d1(0:10), b1%data(-1:18, 0:4))

    b1%data = reshape((/(i, i=1, 100)/), (/4, 20/))

    call allocDouble (d1, b1)

    if (.not. allocated(d1)) error stop 2_4

    if ((lbound(d1,1) /= 1) .or. (ubound(d1,1) /= 80)) error stop 3_4

    do i = 1, 80
        if (.not. precision_r8(d1(i), i*1.0d0)) error stop 4_4
    end do
end
