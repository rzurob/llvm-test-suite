! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE NAME             : fdtio507a7_1kl
!*
!*  PROGRAMMER                 : David Forster (derived from fdtio507a7_1 by Jim Xia)
!*  DATE                       : 2007-08-14 (original: 11/09/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO 
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : DTIO on generics (a test on error condition
!                               that caused by writing more data than specified in
!                               recl=; verify iostat and iomsg)
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
        real(kbase_1), dimension(lbase_1:lbase_2) :: d1 = 1.0 ! really want: "[(real(i),i=lbase_1+1,lbase_2+1)]", was: "(/1.0, 2.0, 3.0/)"
    end type
end module

program fdtio507a7
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
    class (base(4,:,:)), allocatable :: b1 ! tcx: (4,:,:)

    integer stat
    character(8) err

    err = 'no error'

    allocate (base(4,0,2)::b1) ! tcx: base(4,0,2)

    open (1, file='fdtio507a7.data', access='direct', form='unformatted', &
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

    !! the 1st write will eat up 15 spaces
    write (unit, iostat=iostat, iomsg=iomsg) 'abcdefghijklmn '

    if (iostat /= 0) return

    !! there is no more spaces for this write
    write (unit, iostat=iostat, iomsg=iomsg) (dtv%d1+1)
end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 3 changes
! type: base - added parameters (kbase_1,lbase_1,lbase_2) to invoke with (4,0,2) / declare with (4,*,*) - 3 changes
