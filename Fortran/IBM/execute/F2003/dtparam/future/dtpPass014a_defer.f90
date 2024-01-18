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
!*  DATE                       : 06/26/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (An extension of test case
!                               dtpPass014 with the derived type base extended.)
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

        procedure :: at => getValof
        procedure, non_overridable :: find => findAllDataWithCond
        procedure :: print => printBase
    end type

    abstract interface
        logical function match (b1, b2)
        import
            class(base(1)), intent(in) :: b1, b2
        end function
    end interface

    contains

    subroutine printBase (b1)
        class(base(*)), intent(in) :: b1
    end subroutine

    function getValof (b1, index)
        class (base(*)), intent(in) :: b1
        integer, intent(in) :: index

        class(base(1)), allocatable :: getValof

        if (index > b1%n) stop 10

        allocate (getValof)

        getValof%data = b1%data(index)
    end function

    function findAllDataWithCond (b1, b2, cond)
        implicit none
        class(base(*)), intent(in) :: b1
        class(base(1)), intent(in) :: b2
        procedure(match) cond

        class(base(1)), pointer :: findAllDataWithCond(:)

        logical(1) flags(b1%n)

        integer i
        integer, allocatable :: indices(:)

        do i = 1, size(flags)
            flags(i) = cond(b1%at(i), b2)
        end do

        indices = pack ([(i, i = 1, b1%n)], flags)

        allocate (findAllDataWithCond(count(flags)), &
            source = [(b1%at(indices(i)), i = 1, size(indices))])
    end function
end module


module m1
use m
    type, extends(base) :: child (len)
        integer, len :: len

        character(len=len) :: name(n)

        contains

        procedure :: at => getChildValof
        procedure :: print => printChild
    end type

    contains

    subroutine printChild (b1)
        class(child(*,*)), intent(in) :: b1

        do i = 1, b1%n
            write (*, '(g14.5, a)') b1%data(i), b1%name(i)
        end do
    end subroutine


    class(base(1)) function getChildValof (b1, index)
        class (child(*,*)), intent(in) :: b1
        integer, intent(in) :: index

        allocatable getChildValof

        if (index > b1%n) stop 15

        allocate (getChildValof, source = &
            child(1,b1%len)([b1%data(index)],[b1%name(index)]))
    end function
end module


program dtpPass014a
use m1
    class(base(:)), pointer :: b1, b2(:)

    procedure (match) equalWithin10Percent

    character(:), allocatable :: desc(:)

    allocate (character(10) :: desc(200))

    do i = 1, 200
        write (desc(i), '(a, i3.3)') 'test', i
    end do

    allocate (b1, source = child (200, 10) (sqrt([(i*1.0, i = 1, 200)]), &
        desc))

    b2 => b1%find (child(1,0)(11.0, ''), equalWithin10Percent)

    if (.not. associated(b2)) error stop 1_4

    if (size(b2) /= 48) error stop 2_4

    do i = 1, size(b2)
        call b2(i)%print
    end do
end


logical function equalWithin10Percent (b1, b2)
use m
    class(base(1)), intent(in) :: b1, b2

    equalWithin10Percent = &
        abs(b1%data(1) - b2%data(1)) / abs(b2%data(1)) <= &
        0.1
end function
