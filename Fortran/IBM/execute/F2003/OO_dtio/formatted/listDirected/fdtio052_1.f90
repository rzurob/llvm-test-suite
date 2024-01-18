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
! %GROUP: fdtio052_1.f
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
!*  DESCRIPTION                : DTIO generics (use of * as unit in parent
!                               WRITE or PRINT statement will have a value of
!                               OUTPUT_UNIT in child data transfer)
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
        real(4), allocatable :: d1
    end type

    character(*), parameter :: outputUnitErr = &
        'output unit should be OUTPUT_UNIT defined in ISO_FORTRAN_ENV module'

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

program fdtio052_1
use m
    integer stat
    character(200) err

    write (10, *, iostat=stat, iomsg=err) base(100.1)

    if ((stat /= 10) .or. (err /= outputUnitErr)) error stop 1_4
end

!! only call write using * as the unit, it'll fail otherwise
subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, outputUnitErr
use ISO_FORTRAN_ENV
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (unit /= OUTPUT_UNIT) then
        iostat = 10
        iomsg = outputUnitErr
        return
    end if

    if (allocated (dtv%d1)) write (OUTPUT_UNIT, '(f10.2)', &
            iostat=iostat, iomsg=iomsg) dtv%d1
end subroutine
