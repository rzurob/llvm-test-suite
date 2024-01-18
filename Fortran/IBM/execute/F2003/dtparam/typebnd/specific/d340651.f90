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
!*  DATE                       : 08/21/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 340651)
!*
!*
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

        if (b1%n <= 0) then
            computeSizeOf = 0
        else
            computeSizeOf = 4*b1%n
        end if
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
