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
! %GROUP: fdtio510a1.f
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
!*  DATE                       : 11/17/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO on generics (a DTIO for a derived type
!                               calls another DTIO for another derived type)
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
        integer(4), allocatable :: i
    end type

    type dataType
        class (base), allocatable :: data
    end type

    interface write(unformatted)
        subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
        import base
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine

        subroutine unformattedWriteData (dtv, unit, iostat, iomsg)
        import dataType
            class (dataType), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


subroutine unformattedWriteBase (dtv, unit, iostat, iomsg)
use m, only: base
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%i)) write (unit, iostat=iostat, iomsg=iomsg) dtv%i, 'a'
end subroutine

subroutine unformattedWriteData (dtv, unit, iostat, iomsg)
use m, only: dataType, write(unformatted)
    class (dataType), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%data)) write (unit, iostat=iostat, iomsg=iomsg) dtv%data, 'A'
end subroutine

program fdtio510a1
use m
    class (dataType), allocatable :: d1

    integer stat, i1, i2
    character (200) err
    character(2) c1
    character c2

    allocate (d1)
    allocate (d1%data, source=base(100_4))

    open (1, form='unformatted', file='fdtio510a1.data')

    write (1, iostat=stat, iomsg=err) d1, base (200)

    if (stat /= 0) then
        print *, stat, err
        error stop 1_4
    end if

    rewind 1

    read (1) i1, c1, i2, c2

    if ((i1 /= 100) .or. (i2 /= 200)) error stop 2_4

    if ((c1 /= 'aA') .or. (c2 /= 'a')) error stop 3_4

    close (1, status='delete')
end
