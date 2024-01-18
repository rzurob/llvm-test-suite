!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod fdtio057.data
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio057.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
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
!*  DATE                       : 12/01/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO generics (stream access for formatted
!                               write DTIO; try flush statement in DTIO)
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
        integer(8), allocatable :: data(:)
    end type

    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
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
use m, only: base
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    flush (unit, iostat=iostat, iomsg=iomsg)

    if (iostat /= 0) return

    if (allocated (dtv%data)) then
        write (unit, *, iostat=iostat, iomsg=iomsg) dtv%data

        if (iostat /= 0) return
    end if

    flush (unit, iostat=iostat, iomsg=iomsg)

end subroutine

program fdtio057
use m
    class (base), allocatable :: b1, b2(:)

    integer(8) i1(8)

    allocate (b1, source=base((/-1, -2/)))
    allocate (b2(0:1))

    allocate (b2(0)%data(1), source=100_8)
    allocate (b2(1)%data(0:1), source=(/10_8, 20_8/))

    open (10, file='fdtio057.data', access='stream', form='formatted', &
                status='new')

    write (10, *, pos=15) base ((/2,3,1/))

    write (10, *, pos=25) b1

    write (10, *, pos=1) b2

    read (10, *, pos=1) i1(1:3)    !<-- 100, 10, 20
    read (10, *, pos=15) i1(4:6)   !<-- 2, 3, 1
    read (10, *, pos=25) i1(7:8)   !<-- -1, -2

    if (any (i1 /= (/100, 10, 20, 2, 3, 1, -1, -2/))) error stop 1_4

    close (10, status='delete')
end
