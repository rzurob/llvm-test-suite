! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-09 (original: 11/04/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5 Data Transfer Statements
!*                               - C927, POS= and REC= shall not appear at the same time
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
    type :: base (lbase_1) ! lbase_1=3
       integer, len :: lbase_1
        character(lbase_1) :: i
    end type

end module

program C927_001l
use m

    interface read(unformatted)
        subroutine unformattedRead (dtv, unit, iostat, iomsg)
        use m
            class(base(*)), intent(inout) :: dtv ! tcx: (*)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    interface write(unformatted)
        subroutine unformattedWrite (dtv, unit, iostat, iomsg)
        use m
            class(base(*)), intent(in) :: dtv ! tcx: (*)
            integer, intent(in) :: unit
            integer, intent(out) :: iostat
            character(*), intent(inout) :: iomsg
        end subroutine
    end interface

    class(base(:)), allocatable :: b1 ! tcx: (:)
    class(base(:)), pointer     :: b2 ! tcx: (:)

    open(1, file='C927_001l.sdata', access='stream', form='unformatted')
    open(2, file='C927_001l.ddata', recl=5, access='direct', form='unformatted')

    allocate (b1, source= base(3)('IBM')) ! tcx: (3)
    allocate (b2, source= base(3)('FTN')) ! tcx: (3)

    write(1, pos=1, rec=1) b1
    write(2, rec=1, pos=1) b1

    rewind 1

    read (1, pos=1, rec=1 ) b2                !<= pos and rec appear at the same time
    read (2, rec=1, pos=1 ) b2                !<= pos and rec appear at the same time

end program


subroutine unformattedRead (dtv, unit, iostat, iomsg)
use m
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: temp

    read (unit, iostat=iostat, iomsg=iomsg ) temp

    dtv%i = temp

end subroutine


subroutine unformattedWrite (dtv, unit, iostat, iomsg)
use m
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 8 changes
