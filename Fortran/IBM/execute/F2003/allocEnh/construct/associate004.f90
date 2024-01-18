!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 09/07/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the case that the selector is a function
!                               result from a proc-pointer component call, and
!                               it is used as the expr in an intrinsic
!                               assignment.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        real, pointer :: data(:) => null()

        procedure(splitBaseData), pointer :: split => null()
    end type

    abstract interface
        type(base) function splitBaseData (b, num)
        import
            class(base), intent(in) :: b
            integer, intent(in) :: num

            allocatable splitBaseData(:)
        end function
    end interface
end module

program associate004
use m
    procedure(splitBaseData) splitBase

    type(base), allocatable :: b1(:), b2

    logical(4), external :: precision_r4

    allocate (b2, source=base(split=splitBase))

    allocate(b2%data(1005), source=(/(i*1.0_4, i=1,1005)/))

    !! split b2%data into 8 segments: 1-7 with 125 elements, last with 130
    !elements
    associate (x => b2%split(8))
        associate (y =>x(3)%split(3))
            associate (z => y(2)%split(2))
                b1 = z
            end associate

            if ((.not. allocated(b1)) .or. (size(b1) /= 2)) error stop 1_4

            if ((size(b1(1)%data) /= 20) .or. (size(b1(2)%data) /= 21)) &
                    error stop 2_4


            do i = 1, 20
                if (.not. precision_r4(b1(1)%data(i), (291+i)*1.0_4)) &
                    error stop 3_4

                if (.not. precision_r4(b1(2)%data(i), (311+i)*1.0_4)) &
                    error stop 4_4
            end do

            if ((.not. associated(b1(1)%split, splitBase)) .or. &
                (.not. associated(b1(2)%split, splitBase))) error stop 5_4

            if (.not. precision_r4(b1(2)%data(21), 332.0_4)) error stop 6_4

            deallocate (b1(1)%data, b1(2)%data)

            !! reassign b1
            b1 = y

            if ((.not. allocated(b1)) .or. (size(b1) /= 3)) error stop 7_4

            if ((size(b1(1)%data) /= 41) .or. (size(b1(2)%data) /= 41) .or. &
                (size(b1(3)%data) /= 43)) error stop 8_4

            if ((.not. associated(b1(1)%split, splitBase)) .or. &
                (.not. associated(b1(2)%split, splitBase)) .or. &
                (.not. associated(b1(3)%split, splitBase))) error stop 9_4


            do i = 1, 41
                if (.not. precision_r4(b1(1)%data(i), (250+i)*1.0_4)) &
                        error stop 10_4

                if (.not. precision_r4(b1(2)%data(i), (291+i)*1.0_4)) &
                        error stop 11_4

                if (.not. precision_r4(b1(3)%data(i), (332+i)*1.0_4)) &
                        error stop 12_4
            end do

            if (.not. precision_r4(b1(3)%data(42), 374.00_4)) error stop 13_4
            if (.not. precision_r4(b1(3)%data(43), 375.00_4)) error stop 14_4

            deallocate (b1(3)%data, b1(1)%data, b1(2)%data)
        end associate

        b1 = x
    end associate

    if ((.not. allocated(b1)) .or. (size(b1) /= 8)) error stop 15_4

    do i = 1, 8
        if (.not. associated(b1(i)%split, splitBase)) error stop 16_4

        if (size(b1(i)%data) /= 125) then
            if (i == 8) then
                if (size(b1(i)%data) /= 130) error stop 18_4
            else
                error stop 17_4
            end if
        end if

        do j = 1, 125
            if (.not. precision_r4(b1(i)%data(j), ((i-1)*125.00_4+j))) &
                error stop 19_4
        end do
    end do

    do j = 126, 130
        if (.not. precision_r4(b1(8)%data(j), (875.00_4+j))) error stop 20_4
    end do
end


function splitBase (b, num) result(fb)
use m
    class(base), intent(in) :: b
    integer, intent(in) :: num

    type(base), allocatable :: fb(:)

    type A
        real, pointer :: data(:) => null()
    end type

    type(A) tempA(num)
    integer blockSize

    if (.not. associated(b%data)) stop 1
    if (num <= 0) stop 2

    blockSize = size(b%data)/num

    do i = 1, num-1
        allocate (tempA(i)%data(blockSize), &
            source=b%data((i-1)*blockSize+1:i*blockSize))
    end do

    allocate (tempA(num)%data(size(b%data)-(num-1)*blockSize))

    tempA(num)%data = b%data((num-1)*blockSize+1:)

    fb = (/(base(tempA(i)%data, b%split), i=1,num)/)
end function
