! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-14 (original: 10/28/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (IOSTAT and IOMSG values in
!                               DTIO procedures)
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


program fdtio504akl
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

    type(base(4)) :: b1, b2 ! tcx: (4)
    integer stat
    character(20) :: errormsg


    nullify (b1%i)

    open (1, file='fdtio500.data', form='unformatted')

    stat = 100
    errormsg = 'no error'

    write (1, iostat=stat, iomsg=errormsg) b1

    if (stat /= 1) error stop 101_4
    if (errormsg /= 'error occurs') error stop 2_4
end


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated (dtv%i)) then
        write (unit, iostat=iostat, iomsg=iomsg, fmt='(i9)') dtv%i
    else
        write (unit, iostat=iostat, iomsg=iomsg, fmt='(i9)') ISNULL
    end if

    if (iostat > 0) then
        iostat=1
        iomsg='error occurs'
    end if
end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
