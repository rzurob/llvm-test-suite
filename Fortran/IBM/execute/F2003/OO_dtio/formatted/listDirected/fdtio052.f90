!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fdtio052.f
! %VERIFY: fdtio052.out:fdtio052.vf
! %STDIN:
! %STDOUT: fdtio052.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/17/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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

program fdtio052
use m
    print *, base(100.0)

    write (*,*) base (1.0)

    print *, base(null())
end

!! only call write using * as the unit
subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
use ISO_FORTRAN_ENV
    class (base), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (unit /= OUTPUT_UNIT) then
        iostat = 10
        iomsg = 'output unit should be OUTPUT_UNIT defined in ISO_FORTRAN_ENV module'
        return
    end if

    if (allocated (dtv%d1)) write (OUTPUT_UNIT, '(f10.2)', &
            iostat=iostat, iomsg=iomsg) dtv%d1
end subroutine
