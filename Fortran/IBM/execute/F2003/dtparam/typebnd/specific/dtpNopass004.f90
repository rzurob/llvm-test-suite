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
!*  DATE                       : 04/27/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Test the nopass binding of
!                               an abstract type.)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: obj
        contains

        procedure(genObj), nopass, deferred :: reader
    end type

    abstract interface
        function genObj (str, fmt)
            import
            character(*), intent(in) :: str, fmt
            class (obj), allocatable :: genObj
        end function
    end interface
end module


module m1
use m
    type, extends(obj) :: point (n)
        integer, len :: n

        real(4) :: coord(n)

        contains

        procedure, nopass :: reader => genPoint
    end type

    contains

    function genPoint (str, fmt)
        implicit none
        character(*), intent(in) :: str, fmt

        class(obj), allocatable :: genPoint

        integer dim1, iostatus
        real(4), allocatable :: values(:)

        dim1 = 0
        read(fmt, *, iostat=iostatus) dim1

        if (iostatus /= 0) error stop 99_4

        if (dim1 < 2) error stop 100_4

        allocate (values(dim1))

        read(str, *, iostat = iostatus) values

        if (iostatus /= 0) error stop 98_4

        allocate (genPoint, source=point(dim1)(values))
    end function
end module

program dtpNopass004
use m1
    class(obj), pointer :: b1

    type(point(:)), allocatable, target :: p1

    character(:), allocatable :: str, fmt

    logical(4), external :: precision_r4

    p1 = point(1)([1.0])

    b1 => p1

    allocate (character(100) :: str)

    fmt = '3'

    write (str, *) (log(j*1.0), j = 3, 7)

    select type (x => b1%reader(str, fmt))
        type is (point(*))
            p1 = x
            nullify(b1)

        class default
            error stop 10_4
    end select

    !! now verify p1

    if (.not. allocated(p1)) error stop 1_4

    if (p1%n /= 3) error stop 2_4

    do i = 1, 3
        if (.not. precision_r4(p1%coord(i), log((i+2)*1.0_4))) error stop 2_4
    end do
end
