! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-06-19 (original: 11/17/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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
    type base (kb)
       integer, kind :: kb
        real(kb), allocatable :: d1
    end type

    interface write(formatted)
        subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
        import base
            class (base(4)), intent(in) :: dtv
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
    print *, base(4)(100.0)

    write (*,*) base(4) (1.0)

    print *, base(4)(null())
end

!! only call write using * as the unit
subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base
use ISO_FORTRAN_ENV
    class (base(4)), intent(in) :: dtv
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
