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
! %GROUP: fdtio511.f
! %VERIFY: fdtio511.out:fdtio511.vf
! %STDIN:
! %STDOUT: fdtio511.out
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
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO on generics (for components of a derived
!                               type DTIO is invoked based on the component
!                               order)
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
    type A
        integer id
    end type

    type B
        character(20) :: name
    end type

    type base
        type (A) a1
        type (B) b1
    end type
end module


program fdtio511
use m
    interface read(formatted)
        subroutine formattedReadA (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (A), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine formattedReadB (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (B), intent(inout) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(formatted)
        subroutine formattedWriteA (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (A), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine formattedWriteB (dtv, unit, iotype, v_list, iostat, iomsg)
        use m
            class (B), intent(in) :: dtv
            integer, intent(in) :: unit
            character(*), intent(in) :: iotype
            integer, intent(in) :: v_list(:)
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type (base) b1

    print *,  base(A(100), B('xlftest'))

    write (1, *) base (b1=B('xlftest 101'), a1 = A(200))

    close (1)

    read (1, *) b1

    print *, b1

    close(1, status='delete')
end


subroutine formattedReadA (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (A), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%id
end subroutine


subroutine formattedReadB (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (B), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, '(a20)', iostat=iostat, iomsg=iomsg) dtv%name
end subroutine

subroutine formattedWriteA (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (A), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, '(i5)', iostat=iostat, iomsg=iomsg) dtv%id
end subroutine


subroutine formattedWriteB (dtv, unit, iotype, v_list, iostat, iomsg)
use m
    class (B), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, '(a20)', iostat=iostat, iomsg=iomsg)  dtv%name
end subroutine
