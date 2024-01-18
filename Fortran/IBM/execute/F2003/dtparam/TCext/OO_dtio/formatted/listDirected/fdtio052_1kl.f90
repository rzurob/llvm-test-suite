! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio052_1kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio052_1 by Jim Xia)
!*  DATE                       : 2007-06-19 (original: 11/17/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
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

    character(*), parameter :: outputUnitErr = &
        'output unit should be OUTPUT_UNIT defined in ISO_FORTRAN_ENV module'

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

program fdtio052_1
use m
    integer stat
    character(200) err

    write (10, *, iostat=stat, iomsg=err) base(4)(100.1)

    if ((stat /= 10) .or. (err /= outputUnitErr)) error stop 1_4
end

!! only call write using * as the unit, it'll fail otherwise
subroutine formattedWrite (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, outputUnitErr
use ISO_FORTRAN_ENV
    class (base(4)), intent(in) :: dtv
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
