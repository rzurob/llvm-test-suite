! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-13 (original: 11/15/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (dtio generics declared in
!                               module and therefore visible to the programs
!                               that use the module)
!*                               adaptation: exposed kind
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

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        import base
            class (base(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
end module


program fdtio500kl
use m
    type(base(4)) :: b1, b2(2) ! tcx: (4)
    integer stat
    character(20) :: errormsg

    integer(4), target :: i1 = 100

    nullify (b1%i)

    open (1, file='fdtio500kl.data', form='unformatted')

    write (1, iostat=stat, iomsg=errormsg) base(4)(i1), b1 ! tcx: (4)

    if (stat /= 0) error stop 101_4

    rewind 1

    read (1) b2(1), b2(2)

    if (b2(1)%i /= i1) error stop 2_4
    if (associated (b2(2)%i)) error stop 3_4

    close (1, status='delete')
end

subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m, only:base, isnull
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) temp, err

    !! we do not care about the failure of deallocation
    if (associated (dtv%i))   deallocate (dtv%i, stat=err)

    read (unit, iostat=iostat, iomsg=iomsg) temp

    if (iostat /= 0) return

    !! test the NULL pointer
    if (temp == ISNULL) then
        nullify (dtv%i)
    else
        allocate (dtv%i, source=temp)
    end if
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m, only: base, isnull
    class (base(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (associated (dtv%i)) then
        write (unit, iostat=iostat, iomsg=iomsg) dtv%i
    else
        write (unit, iostat=iostat, iomsg=iomsg) ISNULL
    end if
end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
