! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-14 (original: 11/2/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (unformatted I/O for derived
!                               type with array component; use direct access
!                               mode and list item is array)
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
    type base (kbase_1,lbase_1,base_2) ! kbase_1,lbase_1,base_2=4,0,2
       integer, kind :: kbase_1
       integer, len :: lbase_1,base_2
        real(kbase_1), dimension(lbase_1:base_2) :: d1
    end type

    real(4), parameter :: rData(3) = (/1.0, 2.0, 3.0/)
end module

program fdtio507a1kl
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

    class (base(4,:,:)), pointer :: b1(:) ! tcx: (4,:,:)

    integer stat
    character(200) errmsg

    !! create the data file
    call writeData

    !! read in data
    allocate (base(4,0,2)::b1(2)) ! tcx: base(4,0,2)

    read (1, iostat=stat, rec=2, iomsg=errmsg) b1

    if (stat /= 0) error stop 2_4

    print '(3f10.2)', b1(1)%d1
    print '(3f10.2)', b1(2)%d1

    close (1, status='delete')
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
    class (base(4,:,:)), allocatable :: b1(:) ! tcx: (4,:,:)

    integer stat
    character(200) err

    err = 'no error'

    allocate (base(4,0,2)::b1(2)) ! tcx: base(4,0,2)

    b1(1)%d1 = rData
    b1(2)%d1 = rData - 1.0

    open (1, file='fdtio507a1kl.data', access='direct', form='unformatted', &
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
! type: base - added parameters (kbase_1,lbase_1,base_2) to invoke with (4,0,2) / declare with (4,*,*) - 6 changes
