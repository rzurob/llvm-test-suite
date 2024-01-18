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
!*  DATE                       : 08/15/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : miscellaneous (defect 324158)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point8! (k)
        real(8) :: x, y

        contains

        procedure :: negate8 => negateCoordinate8
        generic :: operator(-) => negate8
    end type

    type A8_20! (k, len)
        type(point8), allocatable :: loc(:)
        character(20), allocatable :: name(:)
    end type

    contains

    elemental type(point8) function negateCoordinate8 (p1)
        class(point8), intent(in) :: p1

        negateCoordinate8 = point8(-p1%x, -p1%y)
    end function

    elemental character(10) function genCharVar (i, fmt)
        integer, intent(in) :: i
        character(*), intent(in) :: fmt

        write (genCharVar, fmt) i
    end function
end module

program dtparamConstr051a1
use m
    type(A8_20), allocatable :: b2

    character(:), allocatable :: fmt, result

    logical(4), external :: precision_r8

    fmt = '(i6)'

    b2 = A8_20(-(/(point8(i*1.0d0, i*2.0d0), i=1, 10)/), &
        genCharVar((/(i+1000, i=1,100)/), fmt))

    if (.not. allocated(b2)) error stop 1_4

    if ((.not. allocated(b2%loc)) .or. (.not. allocated(b2%name))) &
        error stop 2_4

    if ((size(b2%loc) /= 10) .or. (size(b2%name) /= 100)) error stop 3_4

    do i = 1, 10
        if (.not. precision_r8(b2%loc(i)%x, -i*1.0d0)) error stop 4_4

        if (.not. precision_r8(b2%loc(i)%y, -i*2.0d0)) error stop 5_4
    end do

    result = repeat(' ', 20)

    do i = 1, 100
        write(result, '(i6)') i+1000

        if (result /= b2%name(i)) error stop 6_4
    end do
end
