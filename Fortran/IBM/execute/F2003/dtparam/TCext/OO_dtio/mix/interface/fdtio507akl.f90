! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio507akl
!*
!*  DATE                       : 2007-08-14 (original: 11/2/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (a basic test on unformatted
!                               I/O with direct access mode)
!                               adaptation: exposed kind, lengths
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
    type base (kbase_1,lbase_1,lbase_2) ! kbase_1,lbase_1,lbase_2=4,0,2
       integer, kind :: kbase_1
       integer, len :: lbase_1,lbase_2
        real(kbase_1), dimension(lbase_1:lbase_2) :: d1
    end type

    real(4), parameter :: rData(3) = (/1.0, 2.0, 3.0/)
end module

program fdtio507akl
use m
    interface read (unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class (base(4,*,*)), intent(inout) :: dtv ! tcx: (4,*,*)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    class (base(4,:,:)), pointer :: b1 ! tcx: (4,:,:)

    integer stat
    character(200) errmsg

    logical precision_r4

    !! create the data file
    call writeData

    !! read in data
    allocate (base(4,0,2)::b1) ! tcx: base(4,0,2)

    read (1, iostat=stat, rec=2, iomsg=errmsg) b1

    if (stat /= 0) error stop 2_4

    !! verify the results.  Note the data written and data read in are changed
    !so that DTIO can be verified
    do i = 1, 3
        if (.not. precision_r4(b1%d1(i-1), rData(i)+3)) error stop 3_4
    end do
end

subroutine writeData
use m
    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class (base(4,*,*)), intent(in) :: dtv ! tcx: (4,*,*)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface
    class (base(4,:,:)), allocatable :: b1 ! tcx: (4,:,:)

    integer stat
    character(200) err

    err = 'no error'

    allocate (base(4,0,2)::b1) ! tcx: base(4,0,2)

    b1%d1 = rData

    open (1, file='fdtio507akl.data', access='direct', form='unformatted', &
            recl=200)

    write (1, iostat=stat, iomsg=err, rec=2) b1

    if (stat /= 0) error stop 101_4
end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base(4,*,*)), intent(inout) :: dtv ! tcx: (4,*,*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg) (dtv%d1+1)
end subroutine


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class (base(4,*,*)), intent(inout) :: dtv ! tcx: (4,*,*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, iostat=iostat, iomsg=iomsg) dtv%d1

    dtv%d1 = dtv%d1 + 2
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1,lbase_1,lbase_2) to invoke with (4,0,2) / declare with (4,*,*) - 6 changes
