!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/10/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               miscellaneous (program seg faults under qsmp)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        complex(8) cx(2)

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    !! subroutine controls precision in writing complex; but in a format
    !similar to listed-directed write
    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character(5), allocatable :: mode

        character, parameter :: separator(2) = (/',', ';'/)

        character currSeparator

        allocate (mode)

        inquire (unit, decimal=mode)

        if (mode == 'COMMA') then
            currSeparator = separator(2)
        else if (mode == 'POINT') then
            currSeparator = separator(1)
        else
            error stop 10_4
        end if

        write (unit, '(2(" (", d18.10, a, d18.10, " )"))', &
            iostat=iostat, iomsg=iomsg) (real(dtv%cx(i)), currSeparator, &
                aimag(dtv%cx(i)), i=1,2,1)
    end subroutine
end module

program dcmlChildWrite008
use m
    type A
        real(8) :: val
        type(base) :: data
    end type

    write (1, '(dc, d18.10, 1x, DT"no effect")') A(1.25d0, base((/2.3d0, 1.2d0/)))
end
