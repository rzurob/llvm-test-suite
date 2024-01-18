! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio505akl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio505a by Jim Xia)
!*  DATE                       : 2007-08-14 (original: 11/5/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO on generics (DTIO applied on component)
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

    type container (kcontainer_1,lcontainer_1) ! kcontainer_1,lcontainer_1=4,2
       integer, kind :: kcontainer_1
       integer, len :: lcontainer_1
        type(base(kcontainer_1)) :: data(lcontainer_1) ! tcx: (kcontainer_1)
    end type
end module


program fdtio505akl
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

    type (container(4,2)) :: co1, co2 ! tcx: (4,2)

    integer stat
    character(200) :: errormsg = ''

    integer(4), target :: i1 = 200

    co1%data(1)%i => i1
    nullify (co1%data(2)%i)

    open (1, file='fdtio505akl.data', form='unformatted')

    !! write co1 to external file
    write (1, iostat=stat, iomsg=errormsg)  co1

    if ((stat /= 0) .or. (errormsg /= '')) error stop 101_4

    rewind 1

    !! now read in co1 and assign these values to co2
    read (1, iostat=stat, iomsg=errormsg) co2

    if ((stat /= 0) .or. (errormsg /= '')) error stop 2_4

    if (co2%data(1)%i /= i1) error stop 3_4

    if (associated (co2%data(2)%i)) error stop 4_4

    close(1, status='delete')
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
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 5 changes
! type: container - added parameters (kcontainer_1,lcontainer_1) to invoke with (4,2) / declare with (4,*) - 1 changes
