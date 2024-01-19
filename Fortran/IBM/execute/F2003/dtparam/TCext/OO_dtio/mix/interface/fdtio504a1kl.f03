! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-14 (original: 11/09/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (error handling during the
!                               child data transfer for different io-unit)
!                               adaptation: exposed kind
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
    type base (kbase_1) ! kbase_1=4
       integer, kind :: kbase_1
        integer(kbase_1), pointer :: i
    end type

    integer(4), parameter :: ISNULL = -999999
end module


program fdtio504a1kl
use m

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    integer stat
    character(8) :: errormsg

    open (1, file='fdtio504a1kl.data', form='unformatted')

    write (1, iostat=stat, iomsg=errormsg) base(4)(null()) ! tcx: (4)


    !! the following values for stat and errmsg is for XLF compiler specific
    if (stat /= 203) error stop 101_4

    if (errormsg /= '1525-203') error stop 2_4
end


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated (dtv%i)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%i
    else
        write (unit+1, iostat=iostat, iomsg=iomsg) ISNULL
    end if
end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
