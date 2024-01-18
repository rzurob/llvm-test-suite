! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/26/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 320316)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        real(8), allocatable :: r1

        contains

        procedure :: writeFormattedA
        generic :: write(formatted) => writeFormattedA
    end type

    contains

    subroutine writeFormattedA (dtv, unit, iotype, v_list, iostat, iomsg)
        class(A), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        write(unit, *) iotype

        if (allocated(dtv%r1)) then
            write (unit,*) dtv%r1
        else
            write (*,*) 'r1 not allocated'
        endif
    end subroutine
end module

use m
    type (A) a1
    integer istat

    a1 = A(1.2)

    !! the following format specification should cause run-time error
    write(*, '(DC, f10.2)', iostat=istat) A(1.2)

    if (istat == 0) stop 1

    !! the following format specification should cause run-time error
    write(*, '(DC, f10.2)', iostat=istat) a1

    if (istat == 0) stop 2
end
