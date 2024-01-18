! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-08-14 (original: 11/5/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : DTIO on generics (test the repositioning of the
!                               file during the child data transfer)
!                               adaptation: exposed kinds, lengths
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
    type A (kA_1,lA_1) ! kA_1,lA_1=4,2
       integer, kind :: kA_1
       integer, len :: lA_1
        real(kA_1) :: d1(lA_1)
    end type

    type base (kbase_1,lbase_1,lbase_2) ! kbase_1,lbase_1,lbase_2=4,2,2
       integer, kind :: kbase_1
       integer, len :: lbase_1,lbase_2
        type (A(kbase_1,lbase_1)) :: data(lbase_2) ! tcx: (kbase_1,lbase_1)
    end type
end module

program fdtio509akl
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

    type (base(4,2,2)) :: b1 ! tcx: (4,2,2)

    b1 = base(4,2,2) ((/A(4,2)((/1.0, 2.0/)), A(4,2) ((/3.0,4.0/))/)) ! tcx: (4,2) ! tcx: (4,2) ! tcx: (4,2,2)

    open (1, access='stream')

    write (1, pos = 10, err=200) b1

    stop 2

200 close (1, status='delete')
end


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class (base(4,*,*)), intent(in) :: dtv ! tcx: (4,*,*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg, pos=100) dtv%data(1)

    if (iostat == 0) stop 101
end subroutine


! Extensions to introduce derived type parameters:
! type: A - added parameters (kA_1,lA_1) to invoke with (4,2) / declare with (4,*) - 3 changes
! type: base - added parameters (kbase_1,lbase_1,lbase_2) to invoke with (4,2,2) / declare with (4,*,*) - 4 changes
