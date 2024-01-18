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
!*  DATE                       : 06/19/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 338170)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer :: n

        real, allocatable :: data(:)
        integer :: len
        character(:), allocatable :: name(:)
    end type
end module

use m
    class(base), allocatable :: b1

    integer itotal
    real localArray(100)
    character(10) :: localStr(100)

    logical(4), external :: precision_r4

    itotal = 30

    localArray = [(i, i = 1, 100)]

    do i = 1, 100
        write (localStr(i), '(a, i3.3, a)') 'data(', i, ')'
    end do

    allocate (b1, source = base (n = itotal, len = localStr%len, &
        data = localArray(:itotal), name=localStr(:itotal)))

    if ((b1%n /= 30) .or. (b1%len /= 10)) error stop 1_4

    if ((.not. allocated(b1%data)) .or. (.not. allocated(b1%name))) error stop 2_4

    if ((size(b1%data) /= 30) .or. (size(b1%name) /= 30) .or. &
        (b1%name%len /= 10)) error stop 3_4

    do i = 1, 30
        if (.not. precision_r4 (b1%data(i), i*1.0_4)) error stop 4_4

        if (b1%name(i) /= localStr(i)) error stop 5_4
    end do
end
