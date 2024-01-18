!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 07/12/2006
!*
!*  DESCRIPTION                : DECIMAL EDIT MODE
!                               Test the value separator (, or ;) for the child
!                               write statement under different decimal edit
!                               mode; use namelist and list-directed write for
!                               child.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type A
        complex, allocatable :: cx

        contains

        procedure :: writeAFmtd
        generic :: write(formatted) => writeAFmtd
    end type

    type B
        complex(8), pointer :: cx

        contains

        procedure :: writeBFmtd
        generic :: write(formatted) => writeBFmtd
    end type

    contains

    subroutine writeAFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(A), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character decMode

        if (allocated(dtv%cx)) then
            inquire (unit, decimal=decMode, iostat=iostat, iomsg=iomsg)

            if (iostat /= 0) return

            if (decMode == 'P') then
                write(unit, *, sign='plus', decimal='Comma', iostat=iostat, &
                        iomsg=iomsg) dtv%cx
            else if (decMode == 'C') then
                write(unit, *, sign='plus', decimal='POINt', iostat=iostat, &
                        iomsg=iomsg) dtv%cx
            else
                error stop 11_4
            end if
        else
            error stop 10_4
        end if
    end subroutine

    subroutine writeBFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(B), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        type x
            complex(8) cx
        end type

        type(x) :: b1

        namelist /base/ b1

        character decMode

        if (.not. associated(dtv%cx)) error stop 20_4

        b1%cx = dtv%cx

        inquire (unit, decimal=decMode, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        if (decMode == 'P') then
            write (unit, base, decimal='COmma', iostat=iostat, iomsg=iomsg)
        else if (decMode == 'C') then
            write (unit, base, decimal='PoInT', iostat=iostat, iomsg=iomsg)
        else
            error stop 21_4
        end if
    end subroutine
end module

module n
use m
    type base
        type(A) :: a1
        type(B) :: b1

        contains

        procedure :: writeBaseFmtd
        generic :: write(formatted) => writeBaseFmtd
    end type

    contains

    subroutine writeBaseFmtd (dtv, unit, iotype, v_list, iostat, iomsg)
        class(base), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        character decMode

        inquire (unit, decimal=decMode, iostat=iostat, iomsg=iomsg)

        if (iostat /= 0) return

        if (decMode == 'P') then        ! point mode
            write (unit, '(dc, dt, dp, /, dt)', iostat=iostat, iomsg=iomsg) &
                dtv%a1, dtv%b1
        else if (decMode == 'C') then   ! comma mode
            write (unit, '(dt, dc, /, dt)', iostat=iostat, iomsg=iomsg, &
                decimal='poInt') dtv%a1, dtv%b1
        else                            ! something is wrong
            error stop 30_4
        end if
    end subroutine
end module

program dcmlChildWrite010
use n
    class (base), allocatable :: b1

    complex(8), target :: cx1

    allocate (b1, source=base (A((1.0, 2.0)), B(cx1)))

    cx1 = (3.0d0, 4.0d0)

    open (10, file='dcmlChildWrite010.data', decimal='Comma')

    write (10, '(ss, DT)') b1

    write (10, '(dp,sp, DT)') b1

end
