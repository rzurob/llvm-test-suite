! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio507kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio507 by Jim Xia)
!*  DATE                       : 2007-08-14 (original: 12/9/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO generics (dtio on direct access mode and
!                               array as the list item)
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

program fdtio507kl
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

    type (base(4)) :: b1(3) ! tcx: (4)
    integer stat1
    character(200) err

    logical precision_r4

    call writeData

    read (1, rec=2, iostat=stat1, iomsg=err) b1

    if (stat1 /= 0) then
        print *, stat1, err
        error stop 2_4
    end if

    !! verify the data read in
    if (allocated (b1(1)%d1)) error stop 3_4

    if ((.not. allocated (b1(2)%d1)) .or. (.not. allocated (b1(3)%d1))) &
            error stop 4_4

    if ((lbound(b1(2)%d1, 1) /= 1) .or. (ubound(b1(2)%d1,1) /= 2)) error stop 5_4

    if ((lbound(b1(3)%d1, 1) /= 0) .or. (ubound(b1(3)%d1,1) /= 0)) error stop 6_4

    if ((.not. precision_r4 (b1(2)%d1(1), 1.0)) .or. &
        (.not. precision_r4 (b1(2)%d1(2), 2.0))) error stop 7_4


    if (.not. precision_r4 (b1(3)%d1(0), 0.7e0_4)) error stop 8_4

    close (1, status='delete')
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

    class (base(4)), pointer :: b1(:) ! tcx: (4)
    real(4), allocatable :: r1(:)
    integer stat
    character(200) errmsg

    allocate (r1(0:0), source=(/.7e0_4/))

    allocate (b1(3))

    allocate (b1(2)%d1(2), source=(/1.0, 2.0/))

    allocate (b1(3)%d1(0:0), source = r1)

    open (1, file='fdtio507kl.data', access='direct', form='unformatted',  &
                        recl=1000)

    write (1, iostat=stat, iomsg=errmsg, rec=2) b1

    if (stat /= 0) then
        print *, stat, errmsg
        error stop 101_4
    end if
end subroutine


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class(base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(12) allocStat
    integer lb, ub

    read (unit, iostat=iostat, iomsg=iomsg)  allocStat

    if (iostat /= 0) return

    !! we only change the value of dtv if the data source from file is allocated
    if (allocStat == ALLOCED) then
        read (unit, iostat=iostat, iomsg=iomsg)  lb, ub

        if (iostat /= 0) return

        if (allocated (dtv%d1))  deallocate (dtv%d1)

        allocate (dtv%d1(lb:ub))

        read (unit, iostat=iostat, iomsg=iomsg) dtv%d1
    end if
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class(base(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (allocated (dtv%d1)) then
        write (unit, iostat=iostat, iomsg=iomsg) ALLOCED, lbound(dtv%d1,1), &
                ubound(dtv%d1,1), dtv%d1
    else
        write (unit, iostat=iostat, iomsg=iomsg) UNALLOCATED
    end if
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
