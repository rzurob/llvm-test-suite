!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio513a.f
! %VERIFY: fdtio513a.out:fdtio513a.vf
! %STDIN:
! %STDOUT: fdtio513a.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 01/04/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO generics (use type-bound to do the IO
!                               during the DTIO; test on internal file)
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

    subroutine printBase (b, unit, stat, err)
        class (base), intent(in) :: b
        integer, intent(in) :: unit

        integer, intent(out) :: stat
        character(*), intent(inout) :: err

        if (allocated (b%id)) then
            write (unit, 100, iostat=stat, iomsg=err) lbound(b%id, 1), &
                                ubound(b%id, 1)

            if (stat /= 0) return

            write (unit, *, iostat=stat, iomsg=err) b%id

        end if

100 format ("bounds: ", 2i5, ";")
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

    call dtv%print(unit, iostat, iomsg)
end subroutine


program fdtio513a
use m
    class (base), allocatable :: b1(:)

    integer(8) i1(0:1), stat

    character (200) msg, err

    i1 = (/-10_8, -1_8/)

    allocate (b1(2), source=(/base((/10_8, 1_8/)), base (i1)/))

    write (msg, *, iostat=stat, iomsg=err) b1

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    print *, msg
end
