! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtio507a7kl
!*
!*  DATE                       : 2007-08-14 (original: 11/09/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (a test on error condition
!                               that caused by writing more data than specified in
!                               recl=; verify iostat and iomsg)
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
    type base (kbase_1,lbase_1,lbase_2) ! kbase_1,lbase_1,lbase_2=4,0,2
       integer, kind :: kbase_1
       integer, len :: lbase_1,lbase_2
        real(kbase_1), dimension(lbase_1:lbase_2) :: d1 = 1.0 ! really want: "[(real(i),i=lbase_1+1,lbase_2+1)]", was: "(/1.0, 2.0, 3.0/)"
    end type
end module

program fdtio507a7kl
use m
    call writeData
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
    character(8) err

    err = 'no error'

    allocate (base(4,0,2)::b1(10)) ! tcx: base(4,0,2)

    open (1, file='fdtio507a7kl.data', access='direct', form='unformatted', &
            recl=20)

    write (1, iostat=stat, iomsg=err, rec=2) b1

    !! the following values are of XLF specific
    if (stat /= 3) error stop 101_4

    if (err /= '1525-003') error stop 2_4

    close (1, status='delete')
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
! type: base - added parameters (kbase_1,lbase_1,lbase_2) to invoke with (4,0,2) / declare with (4,*,*) - 4 changes
