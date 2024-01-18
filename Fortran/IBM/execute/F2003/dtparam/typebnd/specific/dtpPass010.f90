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
!*  DATE                       : 06/08/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : type bound procedure
!                               specific type bound procedure (A test case  on
!                               testing assumed type parameter in type bound and
!                               function result is a type with type parameter.)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (n)
        integer, len :: n

        real :: data(n)

        contains

        procedure :: merge => mergeBase
        procedure :: verify => compare2Arrays
    end type

    contains

    function mergeBase (b1, b2)
        class(base(*)), intent(in) :: b1, b2

        type(base(b1%n+b2%n)) mergeBase

        mergeBase%data(:) = [b1%data, b2%data]
    end function

    logical function compare2Arrays (b1, array2)
        class(base(*)), intent(in) :: b1
        real(4), intent(in) :: array2(b1%n)

        logical(4), external :: precision_r4

        compare2Arrays = .true.

        i = 1

        do while (compare2Arrays .and. i <= b1%n)
            compare2Arrays = compare2Arrays .and. &
                precision_r4(b1%data(i), array2(i))

            i = i + 1
        end do
    end function
end module


program dtpPass010
use m
    type(base(:)), pointer :: b1, b2
    type(base(:)), allocatable :: b3

    logical(4), external :: precision_r4

    allocate (b1, source=base(10)([(i, i=1,10)]))

    allocate (base(20) :: b2)

    b2%data = [(i, i = 11, 30)]

    !! test 1: use associate construct hold the function result
!    associate (x => b1%merge(b2))
    call foo (b1%merge(b2))

    !! test 2: assign the function results to a variable
    b3 = b2%merge(b1)

    if (.not. allocated(b3)) error stop 3_4

    if (b3%n /= 30) error stop 4_4

    if (.not. b3%verify([real :: (i, i=11,30), (i, i=1,10)])) error stop 5_4

    contains

!    associate (x => b1%merge(b2))
    subroutine foo (x)
        type (base(*)), intent(in) :: x

        if (x%n /= 30) error stop 1_4

        do i = 1, 30
            if (.not. precision_r4 (x%data(i), i*1.0_4)) error stop 2_4
        end do

        if (.not. x%verify ([real(4) :: (i, i = 1, 30)])) error stop 12_4
    end subroutine
end
