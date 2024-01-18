!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/21/2007
!*
!*  DESCRIPTION                : miscellaneous (defect 340599, 2nd test case)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real data(n)

        contains

        procedure :: sizeOf => computeSizeOf
    end type

    contains

    pure integer function computeSizeOf (b1)
        class(base(*)), intent(in) :: b1

        select case (b1%n)
            case (:0)
                computeSizeOf = 0

            case default
                computeSizeOf = 4*b1%n
        end select
    end function
end module

use m
    implicit none
    character(:), allocatable :: str
    real, allocatable :: r1(:)
    type(base(:)), allocatable :: b1

    integer n, i

    n = 250

    allocate (base(n) :: b1)

    call random_number (b1%data)

    str = convert2Str (b1)

    r1 = transfer (str, r1)

    if (.not. allocated(r1)) error stop 1_4

    do i = 1, n
        if (r1(i) /= b1%data(i)) error stop 2_4
    end do

    contains

    function convert2Str (b1)
        type (base(*)), intent(in) :: b1

        character(b1%sizeOf()) convert2Str

        convert2Str = transfer(b1%data, convert2Str)
    end function
end
