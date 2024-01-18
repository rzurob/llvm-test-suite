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
!*  DESCRIPTION                : derived type parameter
!                               Use the defined unary operator as the data
!                               source for allocatable components in a structure
!                               constructor.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type point (k)
        integer, kind :: k

        real(k) :: x, y

        contains

        procedure :: negate4 => negateCoordinate4
        procedure :: negate8 => negateCoordinate8
        generic :: operator(-) => negate4, negate8
    end type

    type base (k, len)
        integer, kind :: k
        integer, len :: len

        type(point(k)), allocatable :: loc
        character(len), allocatable :: name
    end type

    type A (k, len)
        integer, kind :: k
        integer, len :: len

        type(point(k)), allocatable :: loc(:)
        character(len), allocatable :: name(:)
    end type

    contains

    elemental type(point(4)) function negateCoordinate4 (p1)
        class(point(4)), intent(in) :: p1

        negateCoordinate4 = point(4) (-p1%x, -p1%y)
    end function

    elemental type(point(8)) function negateCoordinate8 (p1)
        class(point(8)), intent(in) :: p1

        negateCoordinate8 = point(8)(-p1%x, -p1%y)
    end function

    elemental character(10) function genCharVar (i, fmt)
        integer, intent(in) :: i
        character(*), intent(in) :: fmt

        write (genCharVar, fmt) i
    end function
end module

program dtparamConstr051a1
use m
    type(base(4,30)), allocatable :: b1
    type(A(8,20)), allocatable :: b2

    character(:), allocatable :: fmt, result

    logical(4), external :: precision_r4, precision_r8
    type(point(8)) p1(10)

    p1 = (/(point(8)(i*1.0d0, i*2.0d0), i=1, 10)/)

    fmt = '(i6)'

    b1 = base(4,30)(-point(4)(1.2, 2.1), repeat('xlftest dtp work', 3))

    b2 = A(8,20)(-p1, genCharVar((/(i+1000, i=1,100)/), fmt))


    !! verify b1 and b2
    if ((.not. allocated(b1)) .or. (.not. allocated(b2))) error stop 1_4

    if ((.not. precision_r4(b1%loc%x, -1.2_4)) .or. &
        (.not. precision_r4(b1%loc%y, -2.1_4))) error stop 2_4

    if (b1%name /= 'xlftest dtp workxlftest dtp wo') error stop 3_4

    if ((size(b2%loc) /= 10) .or. (size(b2%name) /= 100)) error stop 4_4

    do i = 1, 10
        if (.not. precision_r8(b2%loc(i)%x, -i*1.0d0)) error stop 5_4

        if (.not. precision_r8(b2%loc(i)%y, -i*2.0d0)) error stop 6_4
    end do

    result = 'abc' // repeat(' ', 17)

    do i = 1, 100
        write (result, '(i6)') i+1000

        if (result /= b2%name(i)) error stop 7_4
    end do
end
