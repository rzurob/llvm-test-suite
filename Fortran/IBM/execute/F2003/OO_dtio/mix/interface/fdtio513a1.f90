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
! %GROUP: fdtio513a1.f
! %VERIFY: fdtio513a1.out:fdtio513a1.vf
! %STDIN:
! %STDOUT: fdtio513a1.out
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
!*  DATE                       : 01/04/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO generics (a test case that use type-bounds
!                               for DTIO interface)
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
    type, abstract :: base
        contains

        procedure(printBase), deferred :: print
    end type

    interface
        subroutine printBase (b, unit, iotype, vlist, iostat, iomsg)
        import base
            class (base), intent(in) :: b
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: vlist(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module

module m1
use m
    type, extends(base) :: child
        integer, allocatable :: id

        contains

        procedure :: print => printChild
    end type

    type, extends (child) :: gen3
        character(20) :: name

        contains

        procedure :: print => printGen3
    end type

    contains

    !! assume iotype is 'LISTDIRECTED'
    subroutine printChild (b, unit, iotype, vlist, iostat, iomsg)
        class (child), intent(in) :: b
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: vlist(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') error stop 10_4

        if (allocated (b%id)) then
            write (unit, *, iostat=iostat, iomsg=iomsg) b%id
        end if
    end subroutine

    subroutine printGen3 (b, unit, iotype, vlist, iostat, iomsg)
        class (gen3), intent(in) :: b
        integer, intent(in) :: unit
        character(*), intent(in) :: iotype
        integer, intent(in) :: vlist(:)
        integer, intent(out) :: iostat
        character(*), intent(inout) :: iomsg

        if (iotype /= 'LISTDIRECTED') error stop 20_4

        if (allocated (b%id)) then
            write (unit, '(i8,2a)', iostat=iostat, iomsg=iomsg) b%id, '; ', b%name
        else
            write (unit, *, iostat=iostat, iomsg=iomsg) b%name
        end if
    end subroutine
end module


module n
    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    call dtv%print (unit, iotype, v_list, iostat, iomsg)
end subroutine


program fdtio513a1
use m1
use n
    class (base), allocatable :: b1(:)

    allocate (b1(0:1), source=(/gen3(1, 'xlftest'), gen3(null(), 'team')/))

    print *, b1
end
