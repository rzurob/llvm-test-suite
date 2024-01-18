! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 20090129
!*
!*  DESCRIPTION                : miscellaneous test: a derived type with array
!of real data and a string representation formatted for output.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    implicit none
    type base (n)
        integer, len :: n

        real, private :: data(n)
        character(10*n), allocatable, private :: str

        contains

        procedure :: formatStr
        procedure :: print => printBaseStr
    end type

    interface base
        procedure genBase
    end interface

    contains

    subroutine formatStr (x1, ndigits)
        class(base(*)), intent(inout) :: x1
        integer, intent(in) :: ndigits

        implicit none

        integer i
        character(20) localFmt

        if (.not. allocated(x1%str)) allocate (x1%str)

        if (ndigits >= 9) stop 10

        write (localFmt, '("(f10.",i1,")")') ndigits

        do i = 1, x1%n
            write(x1%str((i-1)*10+1:i*10), localFmt) x1%data(i)
        end do
    end subroutine

    subroutine printBaseStr (b1)
        class(base(*)), intent(in) :: b1

        if (allocated(b1%str)) print *, b1%str
    end subroutine

    function genBase (rdata) result(res)
        real, intent(in) :: rdata(:)

        type(base(size(rdata))) res

        res%data = rdata
    end function
end module

use m
    implicit none
    type (base(:)), pointer :: b1
    type(base(:)), allocatable :: b2

    integer i

    real data1(100)

    allocate (base(10) :: b1)

    data1 = [(i, i = 1, 100)]

    b1 = base (data1(1:10))

    call b1%formatStr (5)
    call b1%print

    b2 = base(data1(11:30))

    call b2%formatStr (6)
    call b2%print

    call b1%formatStr(3)

    call b1%print

    deallocate (b1)
end
