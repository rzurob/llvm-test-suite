! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Use of the namelist for the format control with
!                               DECIMAL= specifier; test read as well; use
!                               stream access file.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: id
        real :: d(2)
    end type

    type, extends(base) :: child
        character(20) :: name
        complex :: cx
    end type
end module


program dcmlCharExprRW003a
use m
    type(base), allocatable :: b1(:), b2(:)

    type(child) c1, c2

    integer pos

    namelist /nml1/ b1, c1

    allocate (b1(10), source=(/(base(i, (/i, i*2/)), i=1,10)/))
    allocate (b2(0:9))

    c1 = child (100, 1.5, 'xlftest F2003', cmplx(-1, -10))

    open (1, file='dcmlCharExprRW003.data', form='formatted', access='stream',&
            decimal='COMMA', delim='APOSTROPHE')

    write (1, nml1, decimal="POINT", pos=100)

    write (1, nml1)

    call readWithPointMode (1, 100, b2, c2)

    !! verify b2, c2
    call verifyData

    deallocate (b2)

    allocate (b2(0:9))

    inquire (1, pos=pos)

    call readWithCommaMode (1, pos, b2, c2)

    call verifyData

    contains

    !! the following subroutine verifies that b1 == b2 and c1 == c2
    subroutine verifyData
        logical(4), external :: precision_r4, precision_x8

        do i = 0, 9
            if (b2(i)%id /= i+1) error stop 1_4

            if (.not. precision_r4(b2(i)%d(1), i*1.0+1)) error stop 2_4
            if (.not. precision_r4(b2(i)%d(2), i*2.0+2)) error stop 3_4

            if (c2%id /= 100) error stop 4_4

            if ((.not. precision_r4(c2%d(1), 1.5)) .or. &
                (.not. precision_r4(c2%d(2), 1.5))) error stop 5_4

            if (c2%name /= 'xlftest F2003') error stop 6_4

            if (.not. precision_x8(c2%cx, cmplx(-1, -10))) error stop 7_4
        end do
    end subroutine
end

subroutine readWithPointMode (unit, pos, b1, c1)
use m
    integer, intent(in) :: unit, pos
    type(base), intent(out) :: b1(10)
    type(child), intent(out) :: c1

    namelist /nml1/ b1, c1

    read(unit, nml1, pos=pos, decimal="POINT")
end subroutine

subroutine readWithCommaMode (unit, pos, b1, c1)
use m
    integer, intent(in) :: unit, pos
    type(base), intent(out) :: b1(10)
    type(child), intent(out) :: c1

    namelist /nml1/ b1, c1

    read(unit, nml1, pos=pos)
end subroutine
