! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio505kl
!*
!*  DATE                       : 2007-08-14 (original: 11/15/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (DTIO on sub-objects)
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

    type container
        type(base(4)) :: data ! tcx: (4)
    end type
end module


program fdtio505kl
use m

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

    type (container) :: co1, co2

    integer stat
    character(200) :: errormsg = ''

    integer(4), target :: i1 = 200

    open (1, file='fdtio505kl.data', form='unformatted')

    write (1, iostat=stat, iomsg=errormsg)  container(base(4)(i1)) ! tcx: (4)

    if ((stat /= 0) .or. (errormsg /= '')) error stop 101_4

    write (1, iostat=stat, iomsg=errormsg)  container (base(4)(null())) ! tcx: (4)

    if ((stat /= 0) .or. (errormsg /= '')) error stop 2_4

    rewind 1

    read (1, iostat=stat, iomsg=errormsg) co1

    if ((stat /= 0) .or. (errormsg /= '')) error stop 3_4

    read (1, iostat=stat, iomsg=errormsg) co2

    if ((stat /= 0) .or. (errormsg /= '')) error stop 4_4

    !! verify the data read in: co1 and co2
    if (associated (co2%data%i)) error stop 5_4

    if (co1%data%i /= i1) error stop 6_4
end


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) temp, err

    !! we do not care about the failure of deallocation
    if (associated (dtv%i))   deallocate (dtv%i, stat=err)

    read (unit, iostat=iostat, iomsg=iomsg) temp

    if (iostat /= 0) return

    !! check for NULL
    if (temp == ISNULL) then
        nullify (dtv%i)
    else
        allocate (dtv%i, source=temp)
    end if
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
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
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 7 changes
