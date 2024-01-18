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
! %GROUP: fdtio507a1.f
! %VERIFY: fdtio507a1.out:fdtio507a1.vf
! %STDIN:
! %STDOUT: fdtio507a1.out
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
!*  DATE                       : 11/2/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : DTIO on generics (unformatted I/O for derived
!                               type with array component; use direct access
!                               mode and list item is array)
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
        real(4), dimension(0:2) :: d1
    end type

    real(4), parameter :: rData(3) = (/1.0, 2.0, 3.0/)
end module

program fdtio507a1
use m
    interface read (unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(inout) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    class (base), pointer :: b1(:)

    integer stat
    character(200) errmsg

    !! create the data file
    call writeData

    !! read in data
    allocate (b1(2))

    read (1, iostat=stat, rec=2, iomsg=errmsg) b1

    if (stat /= 0) error stop 2_4

    print '(3f10.2)', b1(1)%d1
    print '(3f10.2)', b1(2)%d1

    close (1, status='delete')
end

subroutine writeData
use m
    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class (base), intent(in) :: dtv
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
    class (base), allocatable :: b1(:)

    integer stat
    character(200) err

    err = 'no error'

    allocate (b1(2))

    b1(1)%d1 = rData
    b1(2)%d1 = rData - 1.0

    open (1, file='fdtio507a1.data', access='direct', form='unformatted', &
            recl=200)

    write (1, iostat=stat, iomsg=err, rec=2) b1

    if (stat /= 0) error stop 1_4
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg) (dtv%d1+1)
end subroutine


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, iostat=iostat, iomsg=iomsg) dtv%d1

    dtv%d1 = dtv%d1 + 2
end subroutine
