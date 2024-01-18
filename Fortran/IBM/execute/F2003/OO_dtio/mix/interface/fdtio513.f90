!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio513.f
! %VERIFY: fdtio513.data:fdtio513.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: rm -f fdtio513.data
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/04/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : DTIO generics (DTIO used in the type-bound
!                               procedures)
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
    type base
        integer(8), allocatable :: id(:)

        contains

        procedure :: print => printBase
    end type

    interface write(formatted)
        subroutine formattedWrite4Base (dtv, unit, iotype, vlist, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    contains

    subroutine printBase (b, unit)
        class (base), intent(in) :: b
        integer, intent(in) :: unit

        integer stat
        character(200) err

        write (unit, *, iostat=stat, iomsg=err) b

        if (stat /= 0) then
            print *, stat, err

            error stop 10_4
        end if
    end subroutine
end module

subroutine formattedWrite4Base (dtv, unit, iotype, vlist, iostat, iomsg)
use m, only: base
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: vlist(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%id)) then
        write (unit, 100, iostat=iostat, iomsg=iomsg) lbound(dtv%id, 1), &
                                ubound(dtv%id, 1)

        if (iostat /= 0) return

        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%id
    end if

100 format ("bounds: ", 2i5, ";")
end subroutine


program fdtio513
use m
    class (base), allocatable :: b1(:)

    integer(8) i1(0:1)

    i1 = (/-10_8, -1_8/)

    allocate (b1(2), source=(/base((/10_8, 1_8/)), base (i1)/))

    open (10, file='fdtio513.data')

    call b1(1)%print(10)
    call b1(2)%print(10)
end
