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
!*  DATE                       : 06/25/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : derived type parameter
!                               miscellaneous (defect 338423; case 2: remove
!                               type bound print)
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

        write (*, '(5g14.5)') b1%data
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
end module


program dtpPass014a
use m
    class(base(:)), pointer :: b1, b2(:)

    procedure (match) equalWithin10Percent

    allocate (b1, source=base(200)(sqrt([(i*1.0, i = 1, 200)])))
    b2 => b1%find (base(1)(11.0), equalWithin10Percent)

    if (size(b2) /= 48) stop 1

    do i = 1, size(b2)
        call printBase (b2(i))
    end do
end


logical function equalWithin10Percent (b1, b2)
use m
    class(base(1)), intent(in) :: b1, b2

    equalWithin10Percent = &
        abs(b1%data(1) - b2%data(1)) / abs(b2%data(1)) <= &
        0.1
end function
