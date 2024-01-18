! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/22/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (C936: formatted Read for bind(c)
!                               type)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
use iso_c_binding

    type, bind(C) :: bType
        integer(c_short) i
        integer (c_int) j
    end type

    interface read(formatted)
        subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
        import bType
            type (bType), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


program fdtioC936_001a1
use m
    type (bType) b1(2)
    integer stat1
    character(200) error

    open (1, file='fdtioC936_001a1.data', form='formatted')

    write (1, *) -(2**16-10), -100, 10, 100

    rewind 1

    read (1, *, iostat=stat1, iomsg=error) b1

    if (stat1 /= 0) then
        print *, stat1, error
        error stop 1_4
    end if

    !! verify the results
    if (any(b1%i /= (/-10, 10/))) error stop 2_4

    if (any(b1%j /= (/-100, 100/))) error stop 3_4

    close (1, status='delete')
end


subroutine formattedRead (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: bType
    type (bType), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) i, j

    read (unit, *, iostat=iostat, iomsg=iomsg) i, j

    if (iostat /= 0) return

    call convertI4toI2 (i, dtv%i)

    dtv%j = j

    contains

    !! note this subroutine does wield thing if i1 is negative values
    subroutine convertI4toI2 (i1, i2)
        integer(4), intent(in) :: i1
        integer(2), intent(out) :: i2

        integer(2) temp

        temp = i1       !! let compiler takes the last 4 bytes

        i2 = 0
        !! we take the last 15-bits as the value and keep the sign of i1
        call mvbits (temp, 0, 15, i2, 0)

        if (i1 < 0) i2 = -i2
    end subroutine
end subroutine
