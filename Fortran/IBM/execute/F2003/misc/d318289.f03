! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/18/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 318289)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m
    type base
        real(8) :: r1(12)
        procedure(printVal), pointer, pass(b) :: print

        contains

        procedure :: formattedWriteBase
        generic :: write(formatted) => formattedWriteBase
    end type

    abstract interface
        subroutine printVal (unit, b, fmt)
        import
            integer, intent(in) :: unit
            class(base), intent(in) :: b
            character(*), intent(in) :: fmt
        end subroutine

        subroutine writeBase (b, unit, iotype, vlist, iostat, iomsg)
        import
            class(base), intent(in) :: b
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    procedure(writeBase) formattedWriteBase
end module


subroutine formattedWriteBase (b, unit, iotype, vlist, iostat, iomsg)
use m, only: base
    class(base), intent(in) :: b
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(30) fmt

    if (iotype == 'LISTDIRECTED') then
        fmt = '(5g10.4)'
    else !if (iotype(1:2) == 'DT')
        print *, iotype, new_line('a')
        print *, vlist
    end if
end subroutine


use m
    type (base) b1

    b1%r1 = 1.0

    write(*, fmt="(DT'base type'((/12,2/)))") b1
end
