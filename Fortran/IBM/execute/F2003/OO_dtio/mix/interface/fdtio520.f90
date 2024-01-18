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
! %GROUP: fdtio520.f
! %VERIFY: fdtio520.out:fdtio520.vf
! %STDIN:
! %STDOUT: fdtio520.out
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
!*  DATE                       : 01/17/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO generics (list-directed write for a
!                               linked-list; basic test)
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
    type node
        integer(8) :: value
        type (node), pointer :: next => null()
    end type


    interface write(formatted)
        module procedure formattedWrite
    end interface

    contains

    recursive subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        class (node), intent(in) :: dtv
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') return

        if (size(v_list) /= 0) error stop 10_4

        write (unit, '(i10,/)', iostat=iostat, iomsg=iomsg) dtv%value

        if (iostat /= 0) return

        if (associated (dtv%next)) write (unit, *, iostat=iostat, iomsg=iomsg) &
                        dtv%next
    end subroutine
end module


program fdtio520
use m
    type (node) :: list

    integer stat1
    character(200) err

    list%value=1_8

    !! add 2 nodes
    allocate(list%next)
    allocate(list%next%next)

    list%next%value=2
    list%next%next%value=3

    write (*, *, iostat=stat1, iomsg=err) list

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 1_4
    end if
end
