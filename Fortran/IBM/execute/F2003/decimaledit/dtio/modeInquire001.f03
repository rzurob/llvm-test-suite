! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test that the INQUIRE statement during child
!                               write is returning correct values.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        real(8), allocatable :: d1(:)

        contains

        procedure :: writeFormattedA
        generic :: write(formatted) => writeFormattedA
    end type

    contains

    subroutine writeFormattedA (dtv, unit, iotype, v_list, iostat, iomsg)
        class(A), intent(in) :: dtv
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)

        character(20) symbol

        inquire (unit, decimal=symbol, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        write (unit, *, iostat=iostat, iomsg=iomsg, decimal='COMMA') symbol

        if (iostat /= 0) return

        write(unit, '(f16.6)', iostat=iostat, iomsg=iomsg) dtv%d1

        if (iostat /= 0) return

        inquire (unit, decimal=symbol, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        write (unit, *, iostat=iostat, iomsg=iomsg, decimal='POINT') symbol
    end subroutine
end module


program modeInquire001
use m
    write (*,*, decimal='comma') A((/1.0, 2.0, 3.0/))

    write (*, '(DP, DT)', decimal='comma') A((/1.0, 2.0, 3.0/))
end