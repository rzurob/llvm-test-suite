!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/22/2007
!*
!*  DESCRIPTION                : derived type parameter
!                               specific type bound (Type bound is an array of
!                               derived type with type parameter)
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


program dtpPass014
use m
    type(base(2000)) :: b1
    type(base(1)), allocatable :: b2(:)

    procedure (match) equalWithin10Percent

    b1%data = sqrt([(i*1.0, i = 1, 2000)])

    b2 = b1%find (base(1)(11.0), equalWithin10Percent)

    write (*, '(5g14.5)') b2
end


logical function equalWithin10Percent (b1, b2)
use m
    class(base(1)), intent(in) :: b1, b2

    equalWithin10Percent = &
        abs(b1%data(1) - b2%data(1)) / abs(b2%data(1)) <= &
        0.1
end function
