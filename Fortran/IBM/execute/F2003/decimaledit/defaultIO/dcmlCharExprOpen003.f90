!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/24/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the COMMA mode for the derived type object
!                               in list-dirceted output and input.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    logical(4), external :: precision_r8, precision_x6


    contains

    subroutine writeData2Modes (unit, x, fmt)
        integer, intent(in) :: unit
        class(*), intent(in) :: x
        character(*), intent(in) :: fmt

        select type (x)
            type is (real(8))
                call setCommaMode (unit)

                write(unit, '('//fmt//",10x)", advance='no') x

                call setPointMode (unit)

                write(unit, "("//fmt//')') x

            type is (complex(8))
                call setPointMode (unit)

                write(unit, *) x

                call setCommaMode (unit)

                write(unit, *) x

            type is (integer)
                call setCommaMode (unit)

                write (unit, *) x

            class default
                error stop 10_4
        end select
    end subroutine

    subroutine readData2Modes (unit, x)
        integer, intent(in) :: unit
        class(*), intent(out) :: x

        double precision d1
        complex(8) cx1

        select type (x)
            type is (real(8))
                call setCommaMode (unit)

                read(unit, '(d10.2)', advance='no') x
                read(unit, '(10x, DP, d10.2)') d1

                if (.not. precision_r8(d1, x)) error stop 21_4

            type is (complex(8))
                call setPointMode (unit)

                read(unit, *) cx1

                call setCommaMode (unit)

                read(unit, *) x

                if (.not. precision_x6 (x, cx1)) error stop 22_4

            type is (integer)
                read (unit, *) x

            class default
                error stop 20_4
        end select
    end subroutine

    subroutine setPointMode (unit)
        integer, intent(in) :: unit

        character(*), parameter :: POINT = 'POINT'

        open (unit, decimal=''//POINT//'  ')
    end subroutine

    subroutine setCommaMode (unit)
        integer, intent(in) :: unit
        character(*), parameter :: COMMA = 'COMMA'

        character(:), pointer :: c1 => null()

        if (.not. associated(c1)) then
            allocate (c1, source=COMMA//'      ')
        end if

        open (unit, decimal=c1(1:8))
    end subroutine
end module

program dcmlCharExprOpen003
use m
    double precision d1(10), d2(10)

    complex(8) cx1(10), cx2(10)

    d1 = (/(i, i=1,10)/)

    cx1 = cmplx(d1,d1, 8)

    open (10, file='test1', form='formatted', access='stream')

    do i = 1, 10
        call writeData2Modes (10, d1(i), 'D10.2')

        call writeData2Modes (10, i, '')

        call writeData2Modes (10, cx1(i), '')
    end do

    rewind(10)

    iunit = 10

    do i = 1, 10
        call readData2Modes (iunit, d2(i))

        call readData2Modes (iunit, j)

        if (j /= i) error stop 1_4

        call readData2Modes (iunit, cx2(i))
    end do


    !! verify d2 and cx2
    do i = 1, 10
        if (.not. precision_r8 (d1(i), d2(i))) error stop 2_4

        if (.not. precision_x6 (cx1(i), cx2(i))) error stop 3_4
    end do
end

