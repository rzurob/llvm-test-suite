! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio507a4kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio507a4 by Jim Xia)
!*  DATE                       : 2007-08-14 (original: 11/3/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO on generics (REC= shall not appear in a
!                               child data transfer statement)
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
        real(kbase_1), allocatable :: d1(:)
    end type

    character(12), parameter :: UNALLOCATED =  'UNALLOCATED '
    character(12), parameter :: ALLOCED = 'ALLOCATED   '
end module

program fdtio507a4kl
use m
    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class(base(4)), intent(inout) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    type (base(4)) b1 ! tcx: (4)
    integer stat
    character(8) :: err = 'no error'

    call writeData

    read (1, iostat=stat, iomsg=err, rec=2) b1

    if ((stat == 0) .or. (err == 'no error')) error stop 2_4

    print *, stat, err
end


subroutine writeData
use m
    interface write (unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class(base(4)), intent(in) :: dtv ! tcx: (4)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    class (base(4)), pointer :: b1 ! tcx: (4)
    integer stat
    character(8) errmsg

    errmsg = 'no error'

    allocate (b1, source=base(4)((/0.0e0_4/))) ! tcx: (4)

    open (1, file='fdtio507d.data', access='direct', form='unformatted',  &
                        recl=1000)

    write (1, iostat=stat, iomsg=errmsg, rec=2) b1

    !! this write should fail
    if ((stat == 0) .or. (errmsg == 'no error')) error stop 101_4

    print *, stat, errmsg
end subroutine


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class(base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(12) allocStat
    integer lb, ub

    read (unit, iostat=iostat, iomsg=iomsg, rec=1)  allocStat

    if (iostat /= 0) return

    !! we only change the value of dtv if the data source from file is allocated
    if (allocStat == ALLOCED) then
        read (unit, iostat=iostat, iomsg=iomsg, rec=1)  lb, ub

        if (iostat /= 0) return

        if (allocated (dtv%d1))  deallocate (dtv%d1)

        allocate (dtv%d1(lb:ub))

        read (unit, iostat=iostat, iomsg=iomsg, rec=1) dtv%d1
    end if
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class(base(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%d1)) then
        write (unit, iostat=iostat, iomsg=iomsg, rec=1) ALLOCED, lbound(dtv%d1,1), &
                ubound(dtv%d1,1), dtv%d1
    else
        write (unit, iostat=iostat, iomsg=iomsg, rec=1) UNALLOCATED
    end if
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 7 changes
